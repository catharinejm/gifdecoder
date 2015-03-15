#include "decoder.h"

int main() {
    struct bitcursor gif;
    load_file("./emulogic.gif", &gif);

    parse_gif(&gif);
    
    return 0;
}
