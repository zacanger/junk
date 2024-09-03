//! Scheduler for emitting events at a specific time in the future.

use std::collections::VecDeque;
use std::time::{Duration, Instant};

use glutin::event::Event as GlutinEvent;

use crate::event::Event as MintermEvent;

type Event = GlutinEvent<'static, MintermEvent>;

/// ID uniquely identifying a timer.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TimerId {
}

/// Event scheduled to be emitted at a specific time.
pub struct Timer {
    pub deadline: Instant,
    pub event: Event,

    interval: Option<Duration>,
    id: TimerId,
}

/// Scheduler tracking all pending timers.
pub struct Scheduler {
    timers: VecDeque<Timer>,
}

impl Default for Scheduler {
    fn default() -> Self {
        Self { timers: VecDeque::new() }
    }
}

impl Scheduler {
    pub fn new() -> Self {
        Self::default()
    }

    /// Process all pending timers.
    ///
    /// If there are still timers pending after all ready events have been processed, the closest
    /// pending deadline will be returned.
    pub fn update(&mut self, event_queue: &mut Vec<Event>) -> Option<Instant> {
        let now = Instant::now();
        while !self.timers.is_empty() && self.timers[0].deadline <= now {
            if let Some(timer) = self.timers.pop_front() {
                // Automatically repeat the event.
                if let Some(interval) = timer.interval {
                    self.schedule(timer.event.clone(), interval, true, timer.id);
                }

                event_queue.push(timer.event);
            }
        }

        self.timers.get(0).map(|timer| timer.deadline)
    }

    /// Schedule a new event.
    pub fn schedule(&mut self, event: Event, interval: Duration, repeat: bool, timer_id: TimerId) {
        let deadline = Instant::now() + interval;

        // Get insert position in the schedule.
        let mut index = self.timers.len();
        loop {
            if index == 0 {
                break;
            }
            index -= 1;

            if self.timers[index].deadline < deadline {
                break;
            }
        }

        // Set the automatic event repeat rate.
        let interval = if repeat { Some(interval) } else { None };

        self.timers.insert(index, Timer { interval, deadline, event, id: timer_id });
    }
}
