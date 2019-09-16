#include "flow.h"

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
