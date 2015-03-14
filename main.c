#include "decoder.h"

int main() {
    struct cursor gif;
    load_file("./emulogic.gif", &gif);

    parse_gif(&gif);
    
    return 0;
}
