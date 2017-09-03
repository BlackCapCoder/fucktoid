int main () {
  mem[p]++;
  mem[p]++;
  mem[p]++;
  mem[p]++;
  mem[p]++;
  while (mem[p]) {
    mem[p]--;
    p++;
    p++;
    p++;
    mem[p]++;
    mem[p]++;
    p--;
    p--;
    p--;
  }
  p++;
  p++;
  p++;
  putchar(mem[p]);

  return 0;
}
