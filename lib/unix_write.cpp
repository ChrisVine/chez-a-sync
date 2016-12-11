/*
 Copyright (C) 2016 Chris Vine
 
 This file is licensed under the Apache License, Version 2.0 (the
 "License"); you may not use this file except in compliance with the
 License.  You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 implied.  See the License for the specific language governing
 permissions and limitations under the License.
*/

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

extern "C" {

ssize_t a_sync_c_write(int fd, const char* buf, size_t begin, size_t count) {
  ssize_t ret;
  do {
    ret = write(fd, buf + begin, count);
  } while (ret == -1 && errno == EINTR);
  if (ret == -1 && (errno == EAGAIN || errno == EWOULDBLOCK))
    return 0;
  return ret;
}

int a_sync_regular_file_p(int fd) {
  struct stat buf;
  if (fstat(fd, &buf) == -1)
    return -1;
  if (S_ISREG(buf.st_mode))
    return 1;
  return 0;
}

} // extern "C"
