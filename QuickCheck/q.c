#include <stdlib.h>
#include <stdio.h>

typedef struct queue
{
  int inp;  /* input pointer */
  int outp; /* output pointer */
  int size;
  int *buf;
} Queue;

Queue *new(int n)
{
  int *buff = malloc((n+1)*sizeof(int));
  Queue q = {0, 0, n+1, buff};
  Queue *qptr = malloc(sizeof(Queue));
  *qptr = q;
  return qptr;
}

int put (Queue *q, int n)
{
  q->buf[q->inp] = n;
  q->inp = (q->inp + 1) % q->size;
  return n;
}

int get (Queue *q)
{
  int ans = q -> buf[q->outp];
  q->outp = (q->outp + 1) % q ->size;
  return ans;
}

int size(Queue *q)
{
  return (q->inp - q->outp + q->size) % q->size;
}
