// execorder.h - order of execution
//
// Generates a structure by fake executing the entire function.
//
// This structure is able to answer queries like:
// 	- will x always be reached before a
// 	- what possible execution paths can reach a from x

#ifndef EXECORDER_H
#define EXECORDER_H

#include "tac.h"
#include <variant>
#include <vector>
#include <set>

using statement_path_entry = statement *;  // normal entry (not necesairly a conditional, but sparse)
using loop_path_entry = std::size_t;  // loop to previous entry
using path_entry_type = std::variant<statement_path_entry, loop_path_entry>;

template<typename Derived>
struct execution_path_base : public std::vector<path_entry_type> {
	// Additional public methods
	
	bool is_linear() {
		return std::none_of(this->begin(), this->end(), [](const auto& x){return std::holds_alternative<loop_path_entry>(x);});
	}

	// Returns empty if not contained, otherwise list of all points it occurs
	std::set<size_t> contained_at(statement * which) const {
		std::set<size_t> result;
		statement * at = impl()->statement_at(0);
		for (int i = 0; i < this->size() - 1; ++i) {
			statement * next = impl()->statement_at(i+1);
			while (next != at) {
				if (at == which) {
					result.insert(i);
				}

				if (at->cond == nullptr) at = at->next;
				else at = next;
			}
		}
		if (at == which) result.insert(this->size() - 1);
		return result;
	};

	template<typename ...Args>
	execution_path_base(Args&& ...args) : std::vector<path_entry_type>(std::forward<Args>(args)...) {}

private:
	const Derived * impl() const {
		return static_cast<const Derived *>(this);
	}
};

template<typename LoopStorageType>
struct execution_path_impl : public execution_path_base<execution_path_impl<LoopStorageType>> {
	statement * statement_at(std::size_t index) const {
		const auto& ref = this->at(index);

		if (std::holds_alternative<statement_path_entry>(ref)) return std::get<statement_path_entry>(ref);
		else {
			return (*loop_marker_buffer)[std::get<loop_path_entry>(ref)];
		}
	}

	template<typename ...Args>
	execution_path_impl(const LoopStorageType& ref, Args&&... args) : loop_marker_buffer(&ref), execution_path_base<execution_path_impl<LoopStorageType>>(std::forward<Args>(args)...)
	{
	}

private:
	// Public reference
	const LoopStorageType* loop_marker_buffer;
};

using execution_path_internal_marker = std::nullptr_t;

template<>
struct execution_path_impl<void> : public execution_path_base<execution_path_impl<void>> {
	statement * statement_at(std::size_t index) const {
		const auto& ref = this->at(index);

		if (std::holds_alternative<statement_path_entry>(ref)) return std::get<statement_path_entry>(ref);
		else {
			return statement_at(std::get<loop_path_entry>(ref));
		}
	}

	using execution_path_base<::execution_path_impl<void>>::execution_path_base;
};

using execution_path = execution_path_impl<void>;

struct execution_order {
	// Public data types and members
	using data_element = std::vector<execution_path_impl<std::vector<statement *>>>;
	using execution_type = data_element::value_type;

	std::map<statement *, data_element> data;

	// Queries
	bool is_always_reached_before(statement * from, statement * to);

	// From beginning
	std::vector<execution_path> get_all_execution_paths(statement * to);

	// Implemented in terms of from beginning
	std::vector<execution_path> get_all_execution_paths(statement * from, statement * to);

	// Constructor
	execution_order(statement * start) {
		trace(start);

		// Uniqueify everything
		for (auto &[at, entry] : data) {
			std::sort(entry.begin(), entry.end());
			entry.erase(std::unique(entry.begin(), entry.end()), entry.end());
		}
	}
private:
	std::vector<statement *> loop_preref_points;
	
	std::set<statement *> visited;

	template<typename ...Args>
	execution_type new_path(Args&& ...args) {
		auto obj = execution_type{loop_preref_points}; 
		(obj.push_back(std::forward<Args>(args)), ...);
		return obj;
	}

	template<typename ...Args>
	execution_type new_path(execution_type prior, Args&& ...args) {
		auto tail = new_path(std::forward<Args>(args)...);
		auto head = prior;
		head.insert(head.end(), tail.begin(), tail.end());
		return head;
	}

	template<typename ...Args>
	execution_type direct_construct_new_path(Args&& ...args) {
		return execution_type(loop_preref_points, std::forward<Args>(args)...);
	}

	size_t get_loop_point(statement * point) {
		auto iter = std::find(loop_preref_points.begin(), loop_preref_points.end(), point);
		if (iter == loop_preref_points.end()) {
			loop_preref_points.push_back(point);
			return loop_preref_points.size() - 1;
		}
		return std::distance(loop_preref_points.begin(), iter);
	}
	
	void trace(statement * at, statement * from=nullptr) {
		// Uniqueify everything
		for (auto &[at, entry] : data) {
			std::sort(entry.begin(), entry.end());
			entry.erase(std::unique(entry.begin(), entry.end()), entry.end());
		}

		data_element previous = (from == nullptr) ? data_element{new_path(at)} : data[from];

		auto append_entry_with_prefix = [&](statement * at) {
			for (auto &i : data[at])
				i.push_back(at);
		};

		// Continue scanning until at->cond != nullptr
		while (at->cond == nullptr || visited.count(at)) {
			if (visited.count(at)) {
				// Is the current path to at a loop?
				// 
				// If so, the loop point will occur inside the previous array.
				// Otherwise, this is an if-then and we can directly insert the loop.

				// Create the new execution path up to and including the loop, for each potential route to the loop point
				std::cout << "psize " << previous.size() << "\n";
				for (const auto &local_previous : previous) {
					// Is the previous a loop?
					bool is_loop = !local_previous.contained_at(at).empty();
					execution_type up_to_loop = !is_loop ? new_path(local_previous, at) : new_path(local_previous, get_loop_point(at));

					if (!is_loop) continue;

					// Have we already handled this loop case?
					if (std::find(data[at].begin(), data[at].end(), up_to_loop) != data[at].end())
						continue;

					// Add this new path to the data entry for the loop point
					data[at].push_back(up_to_loop);

					std::cout << "loop " << is_loop << "; " << "iterating over " << data.size() << "\n";

					// Check all other currently found paths
					for (const auto& [other_point, other_paths] : data) {
						if (other_point == at) continue;
						data_element to_merge{};
						for (const auto& other_path : other_paths) {
							// Check if the loop point is contained
							auto insertion_points = other_path.contained_at(at);
							if (insertion_points.empty()) continue;

							// Are there more than 1 entries (if so, just complain)
							if (insertion_points.size() > 1) 
								// We've already added a valid loop entry to thing, so let's just continue
								continue;

							// Have we already looped?
							if (is_loop && std::find(other_path.begin(), other_path.end(), execution_type::value_type{get_loop_point(at)}) != other_path.end())
								continue;

							// Get the first one
							size_t index_to_insert = *insertion_points.begin();
							
							// Is this the start?
							if (other_path.size() == 1) {
								// Simply merge the entire thing
								to_merge.push_back(up_to_loop);
								break; // Ignore all other paths
							}
							// Check if the entry there is equal to the loop point
							else if (other_path.statement_at(index_to_insert) == at) {
								// If so, replace that entry with the current one
								execution_type new_path = direct_construct_new_path(up_to_loop);
								new_path.insert(new_path.end(), other_path.begin() + index_to_insert + 1, other_path.end());
								to_merge.push_back(new_path);
							}
							else {
								// Otherwise, add the loop entry after the loop point
								execution_type new_path = direct_construct_new_path(up_to_loop);
								new_path.insert(new_path.end(), other_path.begin() + index_to_insert, other_path.end());
								to_merge.push_back(new_path);
							}
						}

						// Are there new paths to add
						if (to_merge.empty()) continue;

						// Add them
						data[other_point].insert(data[other_point].end(), to_merge.begin(), to_merge.end());
					}
				}

				// Return now
				return;
			}
			
			// Visit this node
			visited.insert(at);
			
			data[at] = previous;
			if (at != from) {
				append_entry_with_prefix(at);
			}

			if (at->next == nullptr || si_ret(*at)) {
				// End of function, return now
				return;
			}
			at = at->next;
		}
		
		// First, normally write the conditional datas
		visited.insert(at);
		
		data[at] = previous;
		if (at != from) {
			append_entry_with_prefix(at);
		}

		// We have reached a conditional, begin tracing in both directions
		if (!visited.count(at->next)) {data[at->next] = previous; append_entry_with_prefix(at->next);}
		if (!visited.count(at->cond)) {data[at->cond] = previous; append_entry_with_prefix(at->cond);}
		std::cout << "vprintA1" << std::endl;
		trace(at->next, at->next);
		std::cout << "vprintA3" << std::endl;
		trace(at->cond, at->cond);
		std::cout << "vprintAdone" << std::endl;

		// Uniqueify everything
		for (auto &[at, entry] : data) {
			std::sort(entry.begin(), entry.end());
			entry.erase(std::unique(entry.begin(), entry.end()), entry.end());
		}
	}
};

#endif
