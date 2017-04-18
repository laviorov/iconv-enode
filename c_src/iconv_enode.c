#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <iconv.h>

#include "ei.h"
extern int ei_tracelevel;
extern void erl_init(void *hp,long heap_size);

struct {
    char *erlang_node;
    int fd;
    ei_cnode ec;
    ei_x_buff x_in;
    ei_x_buff x_out;
    ei_x_buff x_rpc_in;
    ei_x_buff x_rpc_out;
} STATE;


static int handle_msg(erlang_pid *pid);
static void main_message_loop();

static void print(char *fmt, ...) {
    va_list args;

    va_start(args, fmt);
    vprintf(fmt, args);
    printf("\n.\n");
    fflush(stdout);
    va_end(args);
}

int main(int argc, char *argv[]) {
    char *iconv_node;
    char *iconv_host;
    char *cookie;
    struct hostent *host;
    struct in_addr *addr;
    char *fullnodeid;

    if (argc != 6) {
        print("Invalid arguments.");
        exit(1);
    }
    iconv_node = argv[1];
    iconv_host = argv[2];
    STATE.erlang_node = strdup(argv[3]);
    cookie = argv[4];
    ei_tracelevel = atoi(argv[5]);

    /* Attempt to turn off buffering on stdout/err. */
    setvbuf(stdout, NULL, _IONBF, 0);
    setvbuf(stderr, NULL, _IONBF, 0);

    erl_init(NULL, 0);

    ei_x_new(&STATE.x_in);
    ei_x_new(&STATE.x_out);
    ei_x_new(&STATE.x_rpc_in);
    ei_x_new(&STATE.x_rpc_out);

    if ((host = gethostbyname(iconv_host)) == NULL) {
        print("Cannot retrieve host information for %s.", iconv_host);
        exit(3);
    }
    fullnodeid = (char *) malloc(strlen(iconv_node) + 1 + strlen(iconv_host) + 1);
    sprintf(fullnodeid, "%s@%s", iconv_node, iconv_host);
    addr = (struct in_addr *) host->h_addr;

    if (ei_connect_xinit(&STATE.ec, iconv_host, iconv_node, fullnodeid, addr, cookie, 0) ) {
        print("EI initialisation failed: %d (%s)", erl_errno, strerror(erl_errno));
        exit(4);
    }
    print("Iconv Erlang Node '%s' starting.", ei_thisnodename(&STATE.ec));
    if ((STATE.fd = ei_connect(&STATE.ec, STATE.erlang_node)) < 0) {
        print("Cannot connect to parent node '%s': %d (%s)",
              STATE.erlang_node, erl_errno, strerror(erl_errno));
        exit(5);
    }
    print("Iconv Erlang Node started.");
    printf("READY\n"); fflush(stdout);

    main_message_loop();

    print("INFO: Iconv Erlang Node stopped.");
    return 0;
}

static void reconnect() {
    print("Iconv Erlang Node '%s' reconnecting.", ei_thisnodename(&STATE.ec));
    if ((STATE.fd = ei_connect(&STATE.ec, STATE.erlang_node)) < 0) {
        print("FATAL: Cannot reconnect to parent node '%s': %d (%s)",
              STATE.erlang_node, erl_errno, strerror(erl_errno));
        exit(7);
    }
    print("INFO: Iconv Erlang Node reconnected.");
}

static void main_message_loop() {
    erlang_msg msg;

    int running = 1;
    ei_x_buff *x_in = &STATE.x_in;
    ei_x_buff *x_out = &STATE.x_out;
    ei_x_buff *x_rpc_in = &STATE.x_rpc_in;
    ei_x_buff *x_rpc_out = &STATE.x_rpc_out;

    while (running) {
        x_in->index = 0;
        switch (ei_xreceive_msg(STATE.fd, &msg, x_in)) {
        case ERL_ERROR:
        default:
            print("DEBUG: Iconv Erlang Node error in receive: %d (%s)", erl_errno, strerror(erl_errno));
            reconnect();
            break;
        case ERL_TICK:
            if (ei_tracelevel > 2) print("DEBUG: TICK");
            break;
        case ERL_MSG:
            switch (msg.msgtype) {
            case ERL_LINK:
                print("DEBUG: Iconv Erlang Node linked.");
                break;
            case ERL_UNLINK:
            case ERL_EXIT:
                print("DEBUG: Iconv Erlang Node unlinked; terminating.");
                running = 0;
                break;
            case ERL_SEND:
            case ERL_REG_SEND:
                {
                    erlang_pid pid = {{0}, 0, 0, 0};
                    x_in->index = 0;
                    running = handle_msg(&pid);
                    if (running == -1) {
                        /* Ignore messages without a return pid! */
                        running = 1;
                    } else {
                        x_rpc_in->index = x_rpc_out->index = 0;
                        ei_x_encode_empty_list(x_rpc_in); /* empty param list for erlang:is_alive() */
                        if (ei_rpc(&STATE.ec, STATE.fd, "erlang", "is_alive", x_rpc_in->buff, x_rpc_in->index, x_rpc_out) < 0) {
                            print("DEBUG: Iconv Erlang Node error in 'is alive?' rpc to '%s'.", pid.node);
                            reconnect();
                        }
                        if (x_out->index > 0 && ei_send(STATE.fd, &pid, x_out->buff, x_out->index) < 0) {
                            print("FATAL: Iconv Erlang Node error in send to '%s'.", pid.node);
                            exit(8);
                        }
                    }
                }
                break;
            }
            break;
        }
    }
}

static void set_error_msg(ei_x_buff *x_out, const erlang_ref *ref, const char *reason) {
    x_out->index = 0;
    ei_x_encode_version(x_out);
    ei_x_encode_tuple_header(x_out, 3);
    ei_x_encode_atom(x_out, "error");
    ei_x_encode_ref(x_out, ref);
    ei_x_encode_string(x_out, reason);
}

static int handle_msg(erlang_pid *pid) {
    /* 
       in :: stop | {conv, Pid, From, To, Text}
       out :: {error, Ref, Reason} | {ok, Ref, Text}
    */

    ei_x_buff *x_in = &STATE.x_in;
    ei_x_buff *x_out = &STATE.x_out;

    int version, arity, type;
    char atom[MAXATOMLEN+1] = {0};
    int len;
    size_t inlen, outlen, outsize;
    char *from, *to, *textin, *textout, *textinc, *textoutc;
    iconv_t cd;
    erlang_ref ref;

    if (ei_decode_version(x_in->buff, &x_in->index, &version)) {
        print("WARNING: Ignoring malformed message (bad version).");
        return -1;
    }
    if (!ei_decode_atom(x_in->buff, &x_in->index, atom)) {
        if (strcmp(atom, "stop") == 0) {
            print("DEBUG: Iconv Erlang Node stopping normally.");
            x_out->index = 0;
            return 0;
        }
        print("WARNING: Ignoring malformed message (unknown atom).");
        return -1;
    }

    if (ei_decode_tuple_header(x_in->buff, &x_in->index, &arity)) {
        print("WARNING: Ignoring malformed message (not tuple or atom).");
        return -1;
    }
    if (arity != 6) {
        print("WARNING: Ignoring malformed message (not 6-arity tuple).");
        return -1;
    }
    if (ei_decode_atom(x_in->buff, &x_in->index, atom)) {
        print("WARNING: Ignoring malformed message (not atom).");
        return -1;
    }
    if (strcmp(atom, "conv") != 0) {
        print("WARNING: Ignoring malformed message (unknown atom).");
        return -1;
    }
    if (ei_decode_pid(x_in->buff, &x_in->index, pid)) {
        print("WARNING: Ignoring malformed message (second tuple element is not pid).");
        return -1;
    }

    if (ei_decode_ref(x_in->buff, &x_in->index, &ref)) {
        print("WARNING: Ignoring malformed message (third tuple element is not ref).");
        return -1;
    }

    ei_get_type(x_in->buff, &x_in->index, &type, &len);
    from = (char *) malloc((len+1) * sizeof(char));
    from[len] = 0;
    if (ei_decode_binary(x_in->buff, &x_in->index, from, NULL)) {
        free(from);
        print("WARNING: Ignoring malformed message (fourth tuple element for 'conv' not binary).");
        return -1;
    }

    ei_get_type(x_in->buff, &x_in->index, &type, &len);
    to = (char *) malloc((len+1) * sizeof(char));
    to[len] = 0;
    if (ei_decode_binary(x_in->buff, &x_in->index, to, NULL)) {
        free(from); free(to);
        print("WARNING: Ignoring malformed message (fifth tuple element for 'conv' not binary).");
        return -1;
    }

    if ((cd = iconv_open(to, from)) == (iconv_t)-1){
        free(from); free(to);
        if (errno == EINVAL) {
            set_error_msg(x_out, &ref, "The conversion from fromcode to tocode is not supported by the implementation.");
            return 1;
        }
        print("WARNING: Failure to initiate iconv.");
        return -1;
    }

    ei_get_type(x_in->buff, &x_in->index, &type, &len);
    inlen = (size_t) len;
    textin = (char *) malloc(inlen * sizeof(char));
    if (ei_decode_binary(x_in->buff, &x_in->index, textin, NULL)) {
        free(from); free(to); free(textin);
        print("WARNING: Ignoring malformed message (sixth tuple element for 'conv' not binary).");
        return -1;
    }

    outsize = outlen = inlen*2+10;
    textout = (char *) malloc(outlen * sizeof(char));
    textinc = textin;
    textoutc = textout;
    if (iconv(cd, &textinc, &inlen, &textoutc, &outlen) == -1){
        free(from); free(to); free(textin); free(textout);
        iconv_close(cd);
        switch errno {
                case EINVAL: set_error_msg(x_out, &ref, "An incomplete multibyte sequence has been encountered in the input."); break;
                case EILSEQ: set_error_msg(x_out, &ref, "An invalid multibyte sequence has been encountered in the input."); break;
                case E2BIG: set_error_msg(x_out, &ref, "Allocation failure"); break;
                default: set_error_msg(x_out, &ref, "Unknown conversion problem");
            }
        return 1;
    }

    outsize -= outlen;
    x_out->index = 0;
    ei_x_encode_version(x_out);
    ei_x_encode_tuple_header(x_out, 3);
    ei_x_encode_atom(x_out, "ok");
    ei_x_encode_ref(x_out, &ref);
    ei_x_encode_binary(x_out, textout, outsize);

    iconv_close(cd);
    free(from); free(to); free(textin); free(textout);
    return 1;
}
