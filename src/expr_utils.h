// expr_utils.h - various functions to help with expressions
//

#ifndef EXPR_UTILS_H
#define EXPR_UTILS_H

#include <parser.h>
#include <algorithm>

// callv: Invokes the functor with args.
// Returns its return value, except, if func returns void, callv() returns def instead.
template<typename F, typename B, typename... A>
static decltype(auto) callv(F&& func, B&& def, A&&... args)
{
    if constexpr(std::is_invocable_r_v<B,F,A...>) { return std::forward<F>(func)(std::forward<A>(args)...); }
    else                                          { static_assert(std::is_void_v<std::invoke_result_t<F,A...>>);
                                                    std::forward<F>(func)(std::forward<A>(args)...); return std::forward<B>(def); }
}

// for_all_expr() executes the given callbacks on all sub-expressions of the given expression
// (but not on itself). The first param can be: function&, expression&, const expression&.
// The provided callbacks will be executed sequentially. Recursion is depth-first.
// If a callback returns false, the rest of following callbacks will not be executed for that expression.
// If all callbacks return true, testing ends immediately and for_all_expr returns true.
// If a callback returns void, it is treated as if it returned false.
template<typename E, typename... F>
static bool for_all_expr(E& p, bool inclusive, F&&... funcs)
{
    static_assert(std::conjunction_v<std::is_invocable<F,expression&>...>);
    return std::any_of(p.params.begin(), p.params.end(), [&](E& e) { return for_all_expr(e, true, funcs...); })
         || (inclusive && ... && callv(funcs,false,p));
}

#endif
