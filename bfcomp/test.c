#include <stdio.h>
#include <string.h>
#include <stdio.h>

int main () {
#define TAPE_SIZE 65536
char mem[TAPE_SIZE] = {0};
int ptr=0;

printf("Hello World!");
mem[ptr+4]=1;
while(mem[ptr]){
  ptr--;
  while(mem[ptr]){
    while(mem[ptr]){
      ptr--;
      while(mem[ptr]){
        while(mem[ptr]){
          ptr--;
          while(mem[ptr]){
            while(mem[ptr]){
              ptr--;
              while(mem[ptr]){
                mem[ptr]=getchar();
              }
            }
          }
          ptr--;
        }
        ptr--;
      }
      ptr--;
    }
    ptr--;
  }
}
mem[ptr]=14;mem[ptr+1]=0;mem[ptr+2]=16;mem[ptr+3]=0;mem[ptr+4]=17;mem[ptr+5]=0;mem[ptr+6]=18;mem[ptr+7]=0;
while(mem[ptr]){
  mem[ptr]--;
  ptr+=2;
  while(mem[ptr]){
    mem[ptr]--;mem[ptr+1]++;
    ptr+=2;
    while(mem[ptr]){
      mem[ptr]--;mem[ptr+1]++;
      ptr+=2;
      while(mem[ptr]){
        mem[ptr]--;mem[ptr+1]++;mem[ptr+2]++;
        ptr+=2;
        while(mem[ptr]){
          mem[ptr+3]++;
          ptr++;
        }
        ptr+=2;
        while(mem[ptr]){
          mem[ptr-2]++;mem[ptr]--;
          ptr--;
        }
        mem[ptr-1]--;
        ptr--;
        while(mem[ptr]){
          mem[ptr-1]--;mem[ptr]++;
          mem[ptr-9]=0;mem[ptr-7]=0;mem[ptr-6]=0;mem[ptr-5]=0;mem[ptr-4]=0;mem[ptr-3]=0;mem[ptr-2]=0;
        }
        ptr-=3;
      }
      mem[ptr]+=mem[ptr+1];
      mem[ptr+1]=0;
      ptr-=2;
    }
    mem[ptr]+=mem[ptr+1];
    mem[ptr+1]=0;
    ptr-=2;
  }
  mem[ptr]+=mem[ptr+1];
  mem[ptr+1]=0;
  ptr-=2;
}
mem[ptr+1]++;
ptr+=2;
while(mem[ptr]){
  mem[ptr-1]--;
  mem[ptr]=0;
}
ptr--;
while(mem[ptr]){
  mem[ptr]--;
  mem[ptr-1]+=mem[ptr+7];
  mem[ptr+7]=0;
}
ptr--;
while(mem[ptr]){
  printf(" ");
  mem[ptr+1]=0;mem[ptr+2]=0;
  while(mem[ptr]){
    mem[ptr+2]+=mem[ptr];
    mem[ptr]=0;
    ptr+=2;
    while(mem[ptr]){
      mem[ptr-1]+=2;mem[ptr]--;
      while(mem[ptr]){
        mem[ptr-1]++;mem[ptr]--;
        while(mem[ptr]){
          mem[ptr-1]++;mem[ptr]--;
          while(mem[ptr]){
            mem[ptr-1]++;mem[ptr]--;
            while(mem[ptr]){
              mem[ptr-1]++;mem[ptr]--;
              while(mem[ptr]){
                mem[ptr-1]++;mem[ptr]--;
                while(mem[ptr]){
                  mem[ptr-1]++;mem[ptr]--;
                  while(mem[ptr]){
                    mem[ptr-1]++;mem[ptr]--;
                    while(mem[ptr]){
                      mem[ptr-1]++;mem[ptr]--;
                      while(mem[ptr]){
                        mem[ptr]--;mem[ptr+1]++;
                        mem[ptr-1]=1;
                        while(mem[ptr]){
                          mem[ptr-1]--;
                          ptr--;
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      ptr++;
    }
    ptr-=2;
    while(mem[ptr]){
      mem[ptr+1]+=6;
      mem[ptr]+=8*mem[ptr+1];
      mem[ptr]--;
      mem[ptr+1]=0;
      putchar(mem[ptr]);
      mem[ptr]=0;
      ptr--;
    }
  }
}
mem[ptr]=1;
while(mem[ptr]){
  while(mem[ptr]){
    ptr++;
  }
  mem[ptr-1]--;
  ptr--;
  while(mem[ptr]){
    mem[ptr]=getchar();
  }
  mem[ptr]=1;
  while(mem[ptr]){
    ptr++;
  }
  mem[ptr-1]--;
  ptr--;
}
mem[ptr+1]+=8;
ptr++;
while(mem[ptr]){
  while(mem[ptr]){
    ptr++;
  }
  mem[ptr]=1;
  while(mem[ptr]){
    ptr--;
  }
  mem[ptr+1]--;
  ptr++;
}
ptr++;
while(mem[ptr]){
  ptr++;
}
ptr--;
while(mem[ptr]){
  mem[ptr]=0;
  ptr--;
}
mem[ptr-1]++;mem[ptr+127]--;
ptr+=127;
while(mem[ptr]){
  mem[ptr-1]--;mem[ptr]++;
  ptr--;
}
mem[ptr]=1;
while(mem[ptr]){
  ptr++;
}
mem[ptr-1]--;
mem[ptr-257]+=4*mem[ptr-1];
printf("\n");

return 0;
}