#include <memory>
#include <iterator>

/* Tiny transform_iterator for C++. Copyright 2017 © Joel Yliluoma - http://iki.fi/bisqwit/
 * License: MIT

   Example WITHOUT transform_iterator:

      template<typename Func>
      std::vector<int> make_transformed_vector(const std::vector<int>& model, Func&& func)
      {
          std::vector<int> result; // Create named temporary
          std::transform(model.begin(), model.end(), std::back_inserter(result), std::forward<Func>(func));
          return result;
      }
      int main()
      {
          std::vector<int> test{6,10,5};
          std::vector<int> test2 = make_transformed_vector(test, [&](int v) { return v*2; });
          for(auto d: test2) std::printf("%d\n", d);
      }

   Example use, WITH transform_iterator:

      #include "transform_iterator.hh"
      template<typename Func>
      std::vector<int> make_transformed_vector(const std::vector<int>& model, Func&& func)
      {
          // Look ma, no named temporaries!
          return std::vector<int> ( make_transform_iterator(model.begin(), model.end(), std::forward<Func>(func)),
                                    transform_iterator<int>() ); // int is the return type of the functor.
      }
      int main()
      {
          std::vector<int> test{6,10,5};
          std::vector<int> test2 = make_transformed_vector(test, [&](int v) { return v*2; });
          for(auto d: test2) std::printf("%d\n", d);
      }

 */

template<typename R>
struct transform_iterator;
template<typename R>
struct transform_iterator_base
{
    virtual ~transform_iterator_base() {}
protected:
    transform_iterator_base() {}
private:
    friend class transform_iterator<R>;
    virtual bool is_end() const = 0;
    virtual bool eq(const transform_iterator_base<R>&) const = 0;
    virtual void delta(std::ptrdiff_t) = 0;
    virtual R ref() const = 0;
    virtual transform_iterator_base<R>* clone() const = 0;
};

template<typename R>
struct transform_iterator: public std::iterator<std::input_iterator_tag, std::decay_t<R>>
{
    std::unique_ptr<transform_iterator_base<R>> ptr;

    transform_iterator() : ptr{} {}
    transform_iterator(transform_iterator_base<R>* p) : ptr(p) {}
    transform_iterator(const transform_iterator& b)   : ptr(b.ptr ? b.ptr->clone() : nullptr) {}

    bool operator!=(const transform_iterator<R>& b) const { return !operator==(b); }
    bool operator==(const transform_iterator<R>& b) const
    {
        return ptr ? (b.ptr ? ptr->eq(*b.ptr) : ptr->is_end())
                   : (b.ptr ? b.ptr->is_end() : true);
    }
    R operator* () const                { return ptr->ref(); }
    transform_iterator<R>& operator++() { if(ptr) ptr->delta(1); return *this; }
    transform_iterator<R>& operator--() { if(ptr) ptr->delta(-1); return *this; }
    transform_iterator<R>& operator+=(std::ptrdiff_t p) { if(ptr) ptr->delta(p); return *this; }
    transform_iterator<R>& operator-=(std::ptrdiff_t p) { if(ptr) ptr->delta(-p); return *this; }
    transform_iterator<R> operator++(int) { transform_iterator<R> result(*this); ++*this; return result; }
    transform_iterator<R> operator--(int) { transform_iterator<R> result(*this); --*this; return result; }
    transform_iterator<R> operator+(std::ptrdiff_t p) const { transform_iterator<R> result(*this); result+=p; return result; }
    transform_iterator<R> operator-(std::ptrdiff_t p) const { transform_iterator<R> result(*this); result-=p; return result; }
};

template<typename I, typename F, typename R = decltype((*((const F*)nullptr))(*I()))>
struct transform_iterator_spec: public transform_iterator_base<R>
{
    transform_iterator_spec(const I& b, const I& e, F&& f) : transform_iterator_base<R>(),
                                                             cur(b), end(e), func(std::forward<F>(f)) {}
private:
    virtual bool eq(const transform_iterator_base<R>& b) const { return cur == ((const transform_iterator_spec<I,F,R>&)b).cur; }
    virtual bool is_end() const                                  { return cur == end; }
    virtual void delta(std::ptrdiff_t by) { std::advance(cur, by); }
    virtual R ref() const { return func(*cur); }
    virtual transform_iterator_base<R>* clone() const { return new transform_iterator_spec<I,F,R>(cur,end,std::forward<F>(func)); }
private:
    I cur, end;
    F&& func;
};

template<typename I, typename F>
auto make_transform_iterator(const I& begin, const I& end, F&& func)
    -> transform_iterator<decltype(func(*begin))>
{
    using R = decltype(func(*begin));
    return new transform_iterator_spec<I,F,R>(begin, end, std::forward<F>(func));
}

