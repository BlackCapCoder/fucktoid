#include <stdio.h>
#include <string.h>
#include <stdio.h>

int main () {
#define TAPE_SIZE 65536
char mem[TAPE_SIZE] = {0};
int ptr=0;

printf("   0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF\n\x1b" "[1mCurrent character set\x1b" "[m\n0:|                                 !\"#$%&\'()*+,-./0123456789:;<=>?|\n1:|@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~ |\n0:|                                 !\"#$%&\'()*+,-./0123456789:;<=>?|\n1:|@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~ |\n\n2:|                                \xa0" "\xa1" "\xa2" "\xa3" "\xa4" "\xa5" "\xa6" "\xa7" "\xa8" "\xa9" "\xaa" "\xab" "\xac" "\xad" "\xae" "\xaf" "\xb0" "\xb1" "\xb2" "\xb3" "\xb4" "\xb5" "\xb6" "\xb7" "\xb8" "\xb9" "\xba" "\xbb" "\xbc" "\xbd" "\xbe" "\xbf" "|\n3:|\xc0" "\xc1" "\xc2" "\xc3" "\xc4" "\xc5" "\xc6" "\xc7" "\xc8" "\xc9" "\xca" "\xcb" "\xcc" "\xcd" "\xce" "\xcf" "\xd0" "\xd1" "\xd2" "\xd3" "\xd4" "\xd5" "\xd6" "\xd7" "\xd8" "\xd9" "\xda" "\xdb" "\xdc" "\xdd" "\xde" "\xdf" "\xe0" "\xe1" "\xe2" "\xe3" "\xe4" "\xe5" "\xe6" "\xe7" "\xe8" "\xe9" "\xea" "\xeb" "\xec" "\xed" "\xee" "\xef" "\xf0" "\xf1" "\xf2" "\xf3" "\xf4" "\xf5" "\xf6" "\xf7" "\xf8" "\xf9" "\xfa" "\xfb" "\xfc" "\xfd" "\xfe" "\xff" "|\n\n2:|                                \xa0" "\xa1" "\xa2" "\xa3" "\xa4" "\xa5" "\xa6" "\xa7" "\xa8" "\xa9" "\xaa" "\xab" "\xac" "\xad" "\xae" "\xaf" "\xb0" "\xb1" "\xb2" "\xb3" "\xb4" "\xb5" "\xb6" "\xb7" "\xb8" "\xb9" "\xba" "\xbb" "\xbc" "\xbd" "\xbe" "\xbf" "|\n3:|\xc0" "\xc1" "\xc2" "\xc3" "\xc4" "\xc5" "\xc6" "\xc7" "\xc8" "\xc9" "\xca" "\xcb" "\xcc" "\xcd" "\xce" "\xcf" "\xd0" "\xd1" "\xd2" "\xd3" "\xd4" "\xd5" "\xd6" "\xd7" "\xd8" "\xd9" "\xda" "\xdb" "\xdc" "\xdd" "\xde" "\xdf" "\xe0" "\xe1" "\xe2" "\xe3" "\xe4" "\xe5" "\xe6" "\xe7" "\xe8" "\xe9" "\xea" "\xeb" "\xec" "\xed" "\xee" "\xef" "\xf0" "\xf1" "\xf2" "\xf3" "\xf4" "\xf5" "\xf6" "\xf7" "\xf8" "\xf9" "\xfa" "\xfb" "\xfc" "\xfd" "\xfe" "\xff" "|\n");

return 0;
}
