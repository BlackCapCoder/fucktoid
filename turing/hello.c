#include <stdio.h>
#include <string.h>
#include <stdio.h>

int main () {
  #define TAPE_SIZE 65536
  char mem[TAPE_SIZE] = {0};
  int ptr;

memcpy(&mem[ptr],(char[10]){10,70,100,30,10,0,0,0,0,0},10);mem[ptr]=0;mem[ptr+1]+=2;putchar(mem[ptr+1]);mem[ptr+2]+=1;putchar(mem[ptr+2]);mem[ptr+2]+=7;putchar(mem[ptr+2]);putchar(mem[ptr+2]);mem[ptr+2]+=3;putchar(mem[ptr+2]);mem[ptr+3]+=2;putchar(mem[ptr+3]);mem[ptr+1]+=15;putchar(mem[ptr+1]);putchar(mem[ptr+2]);mem[ptr+2]+=3;putchar(mem[ptr+2]);mem[ptr+2]-=6;putchar(mem[ptr+2]);mem[ptr+2]-=8;putchar(mem[ptr+2]);mem[ptr+3]+=1;putchar(mem[ptr+3]);putchar(mem[ptr+4]);

  return 0;
}
