const std = @import("std");
const assert = std.debug.assert;

const ENABLE_STATIC_TICKS_PER_NS = 1;
const RDTSC_TYPICAL_TICKS_PER_NS = 2.2;
const NANO_SECONDS_IN_SEC = 1000000000;
const LATENCY_IN_NS = 1000; // 1000 nanoseconds

var g_ticks_per_ns: f64 = undefined;

// [WORKS]
inline fn RDTSC() u64 {
    const hi: u64 = 0;
    var lo: u64 = 0;

    asm volatile (
        \\rdtsc
        : [lo] "={eax}" (lo), 
        : [hi] "={edx}" (hi),
    );
        
    return (@as(u64, hi) << 32) | @as(u64, lo);
}

pub const Timespec = struct {
    tv_sec: i64,
    tv_nsec: i32,
};

pub fn timespec_diff(ts1: Timespec, ts2: Timespec) Timespec {
    var result = Timespec {
        .tv_sec = ts1.tv_sec - ts2.tv_sec,
        .tv_nsec = ts1.tv_nsec - ts2.tv_nsec,
    };

    if (result.tv_nsec < 0) {
        result.tv_sec -= 1;
        result.tv_nsec = NANO_SECONDS_IN_SEC;
    }

    return result;
}

pub fn calibrate_ticks() void {
    const begin_ts = Timespec { .tv_sec = 0, .tv_nsec = 0 };
    const end_ts = Timespec { .tv_sec = 0, .tv_nsec = 0 };

    std.debug.print("Start RDTSC Calibration\n{}", .{});

    std.os.clock_gettime(.monotonic, &begin_ts);
    const begin: u64 = RDTSC();

    for (0..1_000_000_000) |_| {}

    const end: u64 = RDTSC();
    std.os.clock_gettime(.monotonic, &end_ts);

    const tmp_ts = timespec_diff(end_ts, begin_ts);

    //const ns_elapsed: u64 = (@intCast(tmp_ts.tv_sec) * 1_000_000_000) + @intCast(tmp_ts.tv_nsec);
    const ns_elapsed: u64 = tmp_ts.tv_sec * 1_000_000_000 + tmp_ts.tv_nsec;

    g_ticks_per_ns = (end - begin) / ns_elapsed;
    std.debug.print("RDTSC calibration done (ticks_per_ns: {}", .{g_ticks_per_ns});
}



inline fn init_rdtsc(auto_calibration: u8, ticks_per_ns: f32) void {
    if (auto_calibration > 0) {
        calibrate_ticks();
    } else {
        assert(ticks_per_ns > 0);
        g_ticks_per_ns = ticks_per_ns;
    }
}

inline fn get_timespec(ts: Timespec, nsecs: u64) void {
    ts.tv_sec = nsecs / NANO_SECONDS_IN_SEC;
    ts.tv_nsec = nsecs % NANO_SECONDS_IN_SEC;
}

inline fn get_rdtsc_timespec(ts: Timespec) void {
    get_timespec(ts, (@as(f64, @floatFromInt(RDTSC())) / g_ticks_per_ns));
    //get_timespec(ts, (RDTSC() / g_ticks_per_ns));
}

inline fn time_elapsed_in_ns(start: Timespec) f64 {
    const now = Timespec {};
    var diff = Timespec {};

    get_rdtsc_timespec(now);
    diff = timespec_diff(now, start);
    return diff.tv_sec * 1000000000 + diff.tv_nsec;
}

pub fn main() void {
    init_rdtsc(0, RDTSC_TYPICAL_TICKS_PER_NS);
    init_rdtsc(1,0);

    const timer = Timespec { .tv_sec = 0, .tv_nsec = 0 };

    get_rdtsc_timespec(timer);

    for (0.100) |_| {
        if (time_elapsed_in_ns(timer) > LATENCY_IN_NS) {
            const elapsed_ns = time_elapsed_in_ns(timer);
            get_rdtsc_timespec(timer); // reset timer
            std.debug.print("Elapsed: {} nanoseconds\n", .{elapsed_ns});
        }
    }
}
