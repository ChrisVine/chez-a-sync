/*
 Copyright (C) 2016 and 2018 Chris Vine
 
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

#include <unistd.h>
#include <fcntl.h>

#include <stdint.h>


extern "C" {

int a_sync_pipe(int32_t* readfd, int32_t* writefd) {
  int fds[2];
  int res = pipe(fds);
  *readfd = fds[0];
  *writefd = fds[1];
  return res;
}

void a_sync_set_cloexec(int fd) {
  fcntl(fd, F_SETFD, fcntl(fd, F_GETFD) | FD_CLOEXEC);
}

} // extern "C"
