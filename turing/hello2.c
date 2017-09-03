#include <stdio.h>
#include <string.h>
int main () {
  #define TAPE_SIZE 65536
  char mem[TAPE_SIZE] = {0};
  int ptr;

mem[ptr]+=10;mem[ptr+1]+=mem[ptr]*7;
mem[ptr+2]+=mem[ptr]*10;
mem[ptr+3]+=mem[ptr]*3;
mem[ptr+4]+=mem[ptr];
mem[ptr]=0;ptr++;mem[ptr]+=2;putchar(mem[ptr]);ptr++;mem[ptr]++;putchar(mem[ptr]);mem[ptr]+=7;putchar(mem[ptr]);putchar(mem[ptr]);mem[ptr]+=3;putchar(mem[ptr]);ptr++;mem[ptr]+=2;putchar(mem[ptr]);ptr-=2;mem[ptr]+=15;putchar(mem[ptr]);ptr++;putchar(mem[ptr]);mem[ptr]+=3;putchar(mem[ptr]);mem[ptr]-=6;putchar(mem[ptr]);mem[ptr]-=8;putchar(mem[ptr]);ptr++;mem[ptr]++;putchar(mem[ptr]);ptr++;putchar(mem[ptr]);
  return 0;
}
