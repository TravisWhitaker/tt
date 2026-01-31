#include <errno.h>
#include <unistd.h>
#include <sys/socket.h>

int spin_recv(int fd, void* b, size_t len)
{
  while(1)
  {
    int s = recv(fd, b, len, 0);
    if(s == -1)
      if(errno == EAGAIN || errno == EWOULDBLOCK)
      {
        continue;
      }
      else
      {
        return -1;
      }
    else
    {
      return s;
    }
  }
}

int spin_send(int fd, void* b, size_t len)
{
  size_t left = len;
  void* x = b;

  while(left)
  {
    ssize_t s = send(fd, x, left, 0);
    if(s > 0)
    {
      left = left - s;
      x = x + s;
    }
    else if(s == -1)
    {
      if(errno == EAGAIN || errno == EWOULDBLOCK)
      {
        continue;
      }
      else
      {
        return -1;
      }
    }
  }

  return (int)len;
}
