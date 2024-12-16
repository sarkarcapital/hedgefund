const std = @import("std");

const clock = @cImport({
  @cInclude("clock.h");
});

const LATENCY_IN_NS = 1000;

pub fn main() void {
  clock.init_rdtsc(1, 0);
  var timer: clock.struct_timespec = undefined;
  clock.get_rdtsc_timespec(&timer);

  for (0..10) |_| {
    if(clock.time_elapsed_in_ns(timer) > LATENCY_IN_NS) {
      const elapsed = clock.time_elapsed_in_ns(timer); // get timer
      clock.get_rdtsc_timespec(&timer); // reset timer
      std.debug.print("time elapsed: {} ns\n", .{elapsed});
    } 
  }
}
