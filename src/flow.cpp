#include "flow.h"
#include <limits.h>

int reachable_v(statement * start, statement * end, bool in_all_cases, std::unordered_set<statement *>& seen, int steps=0) {
	std::queue<statement *> to_read;
	to_read.push(start);

	while (!to_read.empty()) {
		auto v = to_read.front();
		to_read.pop();
		if (v == end) return steps;
		if (seen.count(v) != 0) {continue;}
		seen.insert(v);
		++steps;
		
		if (v->next && v->cond) {
			if (in_all_cases) {
				int s1 = reachable_v(v->next, end, true, seen, steps), s2 = reachable_v(v->cond, end, true, seen, steps);
				if (s1 && s2) return std::max(s1, s2);
				return 0;
			}
			to_read.push(v->next);
			to_read.push(v->cond);
		}
		else if (v->next) {
			to_read.push(v->next);
		}
		else if (v->cond) {
			to_read.push(v->cond);
		}
	}

	return 0;
}

int reachable(statement * start, statement * end, bool in_all_cases) {
	std::unordered_set<statement *> v;
	return reachable_v(start, end, in_all_cases, v);
}

// reachable before
// takes more memory and logic
inline bool reachable_before_v(statement * start, statement * before, statement * after, std::unordered_set<statement *> seen={}, int steps=0, int prevfoundstep=INT_MAX) {
	// Scan from start, keeping track of steps and go until the end of the function (or until we've already seen a node)
	
	statement * at = start;
	while (true) {
		// Check to see if we can mark flags:
		// 	Are we at before?
		// 	 Mark prevfoundstep
		// 	Are we at after?
		// 	 Is steps < prevfoundstep?
		// 	  Return false
		// 	 Otherwise keep going
		// 	Are we at the end or a previously seen node?
		// 	 End with true, since we never invalidated the condition
		
		if (at == before)
			prevfoundstep = steps;
		else if (at == after && steps < prevfoundstep)
			return false;
		// Check if we are at the end of the function
		else if (at->next == nullptr || seen.count(at)) {
			// We're done, return true (since we never invalidated the condition)
			return true;
		}

		// At this point, we can increment steps and mark this node as seen
		++steps;
		seen.insert(at);

		// Are we at a conditional
		if (at->cond != nullptr) {
			// Recurse
			return reachable_before_v(at->next, before, after, seen, steps, prevfoundstep) && reachable_before_v(at->cond, before, after, seen, steps, prevfoundstep);
		}
		// Otherwise, continue tracking along
		else {
			at = at->next;
		}
	}
}

// Separated so the default args aren't visible publically
bool reachable_before(statement * start, statement * middle, statement * end) {
	if (start == middle && middle != end) {
		return reachable(start, end, false);
	}

	return reachable_before_v(start, middle, end);
}
