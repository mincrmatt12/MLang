// flow.h -- access flow tracing
//
// creates accessinfo structs

#ifndef FLOW_H
#define FLOW_H

#include "tac.h"
#include <variant>
#include <set>

// define an access info struct
// this struct contains information about any number of registers.
// since literals and idents really can't be tracked or are constant anyways
//
// because of this, the access crap only needs to deal with registers.

using parameter_source = std::size_t;
using undefined_source = std::nullptr_t;
using source_type      = std::variant<undefined_source, parameter_source, statement *>;

struct access_data {
	using state_type = std::vector<std::set<source_type>>; // A vector (for multiple sets only, not times) of _possible_ sources.

	state_type parameters;
	// read parameters show what is a source
	// write params show what used this as a source
	// parameters that are not regs get undefined source

	state_type everything;
	// snapshot of sources for all registers at this moment
};

struct access_info {
	std::map<statement *, access_data> data;

private:
	void trace(statement * where, access_data::state_type&& state, bool follow_copies, bool include_ifnz) {
		auto& mydata = data[where]; // value initialized a new access_data

		int changes = 0;
		// create the everything vec
		for (std::size_t r=0; r < state.size(); ++r) {
			for (const auto& s : state[r]) {
				changes += mydata.everything[r].insert(s).second;
			}
		}

		if (follow_copies && (si_mov(*where) || si_cast(*where))) {
			if (!changes) return;

			// make sure both arguments are registers
			if (ai_reg(where->lhs()) && ai_reg(where->rhs())) {
				state[where->lhs().num] = state[where->rhs().num];
			}
			// if the target is a constant, something has gone terribly wrong
			if (ai_num(where->lhs())) {
				throw std::runtime_error("assignment to a constant");
			}
			if (ai_reg(where->lhs())) {
				state[where->lhs().num] = {where};
			}
		}
		else {
			int i = 0;
			where->for_all_read([&](auto &reg) {
					int index = i++;

					if (!ai_reg(reg)) return;
					int regno = reg.num;
					changes += std::count_if(state[regno].begin(), state[regno].end(), [&](const auto& source){
							auto writer_ = std::get_if<statement *>(source);
							auto writer = writer_ == nullptr ? nullptr : *writer_;
							int i2 = 0;

							if (writer != nullptr) {
								writer->for_all_write([&](auto &reg2){
									int index2 = i2++;
									if (reg2 == reg) {
										++changes;
										data[writer].params[index2].insert(where);
									}
								});
							}

							return mydata.parameters[index].insert(source).second;
					});
					if (!changes) return;
					where->for_all_write([&](auto &reg){ if (ai_reg(reg)) state[reg.num] = { where };});

					if (include_ifnz && si_ifnz(*where) && ai_reg(where->lhs())) {state[where->lhs().num] = {where};}
			});
		}

		if (where->cond != nullptr) {
			trace(where->cond, where->next == nullptr ? std::move(state) : access_data::state_type(state), follow_copies, include_ifnz);
		}
		if (where->next != nullptr) {
			trace(where->next, std::move(state), follow_copies, include_ifnz);
		}
	}

public:
	access_info(const compilation_unit &cu, bool follow_copies, bool include_ifnz) {
		// Create a vectir of all the statements, instead of using traverse_f every single time
		auto all_statements = traverse_v(cu.start);

		std::size_t max_register_number = cu.counter; 

		for (const auto&s : all_statements) {
			data.emplace(s, access_data{
					access_data::state_type(s->params.size()),
					access_data::state_type(max_register_number)
			});
		}

		access_data::state_type state(max_register_number);
		for (std::size_t r = 0; r < max_register_number; ++r) {
			if (r < cu.num_params) state[r].insert(parameter_source{r});
			else                   state[r].insert(undefined_source{});
		}

		trace(cu.start, std::move(state), follow_copies, include_ifnz);
	}
};

#endif //FLOW_H
