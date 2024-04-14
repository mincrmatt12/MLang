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

// Separated so the default args aren't visible publically
bool reachable_before(statement * start, statement * middle, statement * end) {
	if (start == middle && middle != end) {
		return reachable(start, end, false);
	}

	std::unordered_set<statement *> visited{};
	std::queue<statement *> q;

	q.push(start);
	visited.insert(start);

	bool exclusive = false;

	while (!q.empty()) {
		auto * v = q.front();
		q.pop();

		if (v == middle)
			return true;
		if (v == end) {
			if (!exclusive && start == end) {
				exclusive = true;
			}
			else {
				continue;
			}
		}

		if (v->next) {
			if (visited.insert(v->next).second)
				q.push(v->next);
		}
		if (v->cond) {
			if (visited.insert(v->cond).second)
				q.push(v->cond);
		}
	}

	return false;
}
