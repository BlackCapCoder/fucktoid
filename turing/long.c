#include <stdio.h>
#include <string.h>
#include <stdio.h>

int main () {
  #define TAPE_SIZE 65536
  char mem[TAPE_SIZE] = {0};
  int ptr;

memcpy(&mem[ptr],(char[10]){0,1,1,1,1,2,0,0,0,0},10);ptr+=4;
while(mem[ptr]){
ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=3;mem[ptr+6]+=1;mem[ptr+7]+=1;mem[ptr+8]+=1;mem[ptr+9]+=1;mem[ptr+10]+=2;ptr+=9;
while(mem[ptr]){
ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=3;mem[ptr+6]+=1;mem[ptr+7]+=1;mem[ptr+8]+=1;mem[ptr+9]+=1;mem[ptr+10]+=2;ptr+=9;
while(mem[ptr]){
ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=3;mem[ptr+6]+=1;mem[ptr+7]+=1;mem[ptr+8]+=1;mem[ptr+9]+=1;mem[ptr+10]+=2;ptr+=9;
while(mem[ptr]){
ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=3;mem[ptr+6]+=mem[ptr+5]*5;memcpy(&mem[ptr+5],(char[2]){0,0},2);
}
ptr-=2;
}
mem[ptr+1]=0;ptr-=4;
}
ptr-=2;
}
mem[ptr+1]=0;ptr-=4;
}
ptr-=2;
}
mem[ptr+1]=0;ptr-=4;
}
ptr-=2;
}
putchar(mem[ptr+1]);

  return 0;
}
