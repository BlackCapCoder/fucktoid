var PREV = 0 // Move pointer left
  , NEXT = 1 // Move pointer right and flip
  , SKIP = 2 // Skip next instruction if false
  , HALT = 3 // End execution

  , arr   = []
  , ptr   = 0
  , cur   = 0

  , insts = [PREV,NEXT,SKIP,PREV,SKIP,HALT,PREV,SKIP];

// Initialize program memory
for (let i = 0; i < 10000; i++)
  arr[i] = false;


// Evaluate the program
while (true) {
  let op = insts[cur];

  if (op == HALT) break;
  if (op == PREV) ptr = ptr == 0 ? arr.length-1 : ptr-1;
  if (op == NEXT) arr[ptr = (ptr+1)%arr.length] = !arr[ptr];
  if (op == SKIP
    && !arr[ptr]) cur++;

  cur = (cur+1) % insts.length;
}

console.log('Done');
