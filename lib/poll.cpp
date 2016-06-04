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

#include <unistd.h>
#include <poll.h>
#include <errno.h>

#include <stdint.h>

#include <map>

extern "C" {

// these are in the 'scheme' binary and can be statically linked
// against
int Sactivate_thread(void);
void Sdeactivate_thread(void);
void Slock_object(void*);
void Sunlock_object(void*);

// this function returns the number of pollfd elements inserted in
// out_table.  The calling code should allocate an uninitialised
// bytevector of size sizeof(pollfd) times the sum of the number of
// file descriptors in readfds and writefds.  The a_sync_sizeof_pollfd
// function is available to help with this.  If there is both a read
// and write watch on the same descriptor then we would waste a small
// amount of space in allocating the bytevector, but that is uncommon
// and not worth troubling ourselves over - this approach results in a
// nice clean interface for the chez FFI without having to worry about
// memory management.  Since the number of pollfd elements is
// returned, if the user really is worried she can always copy
// out_table to a smaller bytevector herself and dispose of out_table
// after doing so.  (Note, we cannot return an array of pollfd's
// directly, from which chez scheme's FFI can construct a bytevector
// for us, because the first NULL byte encountered will be treated by
// the FFI as terminating the array.)
int a_sync_make_poll_table(const int32_t* readfds, const int32_t* writefds,
			   pollfd* out_table) {
  std::map<int, pollfd> fd_map;

  if (readfds) {
    for (int index = 0; readfds[index] != -1; ++index) {
      int fd = readfds[index];
      pollfd elem;
      elem.fd = fd;
      elem.events = POLLIN | POLLPRI;
      elem.revents = 0;
      // if there are duplicates we can let a duplicate fall aside
      fd_map.emplace(fd, elem);
    }
  }

  if (writefds) {
    for (int index = 0; writefds[index] != -1; ++index) {
      int fd = writefds[index];
      pollfd elem;
      elem.fd = fd;
      elem.events = POLLOUT;
      elem.revents = 0;
      auto res = fd_map.emplace(fd, elem);
      // if we already have the descriptor in the map because it also
      // appeared in readfds, we just need to add POLLOUT to the watch
      if (!res.second)
	res.first->second.events |= POLLOUT;
    }
  }
  // now fill the output table
  int index = 0;
  for (auto&& elt: fd_map) {
    out_table[index++] = elt.second;
  }
  return index;
}

// a_sync_poll() is a simplified interface for C poll(), to be
// accessed from scheme wrapper code (in particular, by the 'poll'
// procedure in pole.ss).  poll_table is an array of pollfd objects,
// as constructed by a_sync_make_poll_table().  out_read_bv,
// out_write_bv and out_except_bv are bytevectors which are out
// parameters and when this function returns will comprise a -1
// terminated array of the file descriptors which poll() has found to
// be ready for reading, writing or in exceptional condition
// respectively.  out_read_size, out_write_size and out_except_size
// indicate the size of the passed in bytevectors (in units of
// int32_t), and would normally be the number of the respective
// components in the poll table file descriptors plus 1.  However,
// that need not be the case: these bytevectors will be filled up so
// far as space is available, and where there is insufficient space
// for any of them, the ones not dealt with can be attended to in the
// next iteration of this function.  Only file descriptors for which
// there is a read watch in poll_table can feature in the bytevector
// of file descriptors in exceptional condition (out_except_bv), but
// as exceptional conditions are rare to non-existent it would not
// normally be necessary to make it as large as the number of read
// watches in poll_table.  The bytevectors must be at least 4 bytes
// long (which when combined with an out_read_size, out_write_size or
// out_except_size value of 1 represents an "ignore" status) to allow
// space for a terminating -1.  This function returns 0 if file
// descriptors were found by poll() to be ready, 1 on timeout, 2 on
// EINTR or EAGAIN and -1 on any other error (the errno may be
// obtained by calling a_sync_errno()).  If timout is negative no
// timeout is applied.  Otherwise the timeout argument indicates the
// duration of the timeout in milliseconds.
int a_sync_poll(pollfd* poll_table, int table_size, int32_t* out_read_bv,
		int out_read_size, int32_t* out_write_bv, int out_write_size,
		int32_t* out_except_bv, int out_except_size, int timeout) {

  // nothing in these C functions for poll() can throw
  Slock_object(poll_table);
  Slock_object(out_read_bv);
  Slock_object(out_write_bv);
  Slock_object(out_except_bv);
  Sdeactivate_thread();
  int res = poll(poll_table, table_size, timeout);
  Sactivate_thread();
  Sunlock_object(poll_table);
  Sunlock_object(out_read_bv);
  Sunlock_object(out_write_bv);
  Sunlock_object(out_except_bv);

  if (res == 0) {
    out_read_bv[0] = -1;
    out_write_bv[0] = -1;
    out_except_bv[0] = -1;
    return 1;
  }
  if (res == -1) {
    out_read_bv[0] = -1;
    out_write_bv[0] = -1;
    out_except_bv[0] = -1;
    if (errno == EINTR || errno == EAGAIN || errno == EWOULDBLOCK) return 2;
    return -1;
  }
  int table_index = 0;
  int out_read_index = 0;
  int out_write_index = 0;
  int out_except_index = 0;
  // leave space for terminating -1
  --out_read_size, --out_write_size, --out_except_size;
  // now read the results of calling poll in poll_table
  for (; table_index < table_size; ++table_index) {
    // if there is a POLLPRI event, get the user to handle that first:
    // we can always attend to normal data on the next iteration of
    // the event loop.
    pollfd current = poll_table[table_index];
    if (current.revents & POLLPRI) {
      if (out_except_index < out_except_size)
	out_except_bv[out_except_index++] = current.fd;
      continue;
    }
    // if there is an error or hup condition, and there is a read
    // watch, send it to the read watch
    if ((current.revents & (POLLERR | POLLHUP | POLLNVAL))
	&& (current.events & POLLIN)) {
      if (out_read_index < out_read_size)
	out_read_bv[out_read_index++] = current.fd;
      continue;
    }
    // if there is an error or hup condition, and there is no read
    // watch, send it to the write watch (POLLHUP shouldn't occur in
    // these circumstances, but ...)
    if ((current.revents & (POLLERR | POLLHUP | POLLNVAL))
	&& (current.events & POLLOUT)) {
      if (out_write_index < out_write_size)
	out_write_bv[out_write_index++] = current.fd;
      continue;
    }
    if (current.revents & POLLIN) {
      if (out_read_index < out_read_size)
	out_read_bv[out_read_index++] = current.fd;
    }
    if (current.revents & POLLOUT) {
      if (out_write_index < out_write_size)
	out_write_bv[out_write_index++] = current.fd;
    }
  }
  out_read_bv[out_read_index] = -1;
  out_write_bv[out_write_index] = -1;
  out_except_bv[out_except_index] = -1;

  return 0;
}

int a_sync_sizeof_pollfd(void) {
    return sizeof(pollfd);
}

} // extern "C"
