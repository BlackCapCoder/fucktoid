#include <stdio.h>
#include <string.h>
#include <stdio.h>

int main () {
#define TAPE_SIZE 65536
char mem[TAPE_SIZE] = {0};
int ptr=0;

mem[ptr]=8;
while(mem[ptr]){
  mem[ptr]--;mem[ptr+1]--;
  ptr++;
  while(mem[ptr]){
    mem[ptr]--;mem[ptr+1]--;
    ptr++;
    while(mem[ptr]){
      mem[ptr]--;
      mem[ptr+1]=0;
    }
    ptr--;
  }
  ptr--;
}
mem[ptr+1]+=8;
mem[ptr]+=10*mem[ptr+1];
mem[ptr+1]=0;
mem[ptr+1]+=mem[ptr];mem[ptr+2]+=mem[ptr];
mem[ptr+1]--;
mem[ptr]=0;
putchar(mem[ptr+1]);
mem[ptr+2]+=251;
putchar(mem[ptr+2]);

return 0;
}
