#pragma once
#include <tuple>

namespace kaixo {
    template<class>
    struct member_signature;
    template<class Return, class T, class... Args>
    struct member_signature<Return(T::*)(Args...) const> {
        using type = Return(Args...);
    };

    template<class Return, class T, class... Args>
    struct member_signature<Return(T::*)(Args...)> {
        using type = Return(Args...);
    };

    template<class, class = void>
    struct lambda_signature;
    template<class _Fx>
    struct lambda_signature<_Fx, std::void_t<decltype(&_Fx::operator())>> {
        using type = member_signature<decltype(&_Fx::operator())>::type;
    };

    template<class>
    struct funptr_to_type;
    template<class Return, class ...Args>
    struct funptr_to_type<Return(*)(Args...)> {
        using type = Return(Args...);
    };

    template<class, size_t>
    struct last_n_args;
    template<class Return, class ...Args, size_t N>
    struct last_n_args<Return(Args...), N> {
        template<std::size_t... I>
        static inline Return(*last_n(std::index_sequence<I...>))(std::tuple_element_t<sizeof...(Args) - N + I, std::tuple<Args...>>...) {};
        using type = typename funptr_to_type<decltype(last_n(std::make_index_sequence<N>{}))>::type;
    };

    template<class, size_t>
    struct first_n_args;
    template<class Return, typename ...Args, size_t N>
    struct first_n_args<Return(Args...), N> {
        template<std::size_t... I>
        static inline Return(*first_n(std::index_sequence<I...>))(std::tuple_element_t<I, std::tuple<Args...>>...) {};
        using type = typename funptr_to_type<decltype(first_n(std::make_index_sequence<N>{}))>::type;
    };

    template<class Func, class ...Tys>
    concept are_first_n = requires(typename first_n_args<Func, sizeof...(Tys)>::type func, Tys&&...tys) {
        func(std::forward<Tys>(tys)...);
    };

    template<class Return, class ...Args>
    class function_storage {
    public:
        virtual Return call(Args&&...) const = 0;
        size_t ref_count = 1;
    };

    template<class, class>
    class typed_function_storage;
    template<class Func, class Return, class ...Args>
    class typed_function_storage<Func, Return(Args...)> : public function_storage<Return, Args...> {
        Func function;

    public:
        typed_function_storage(Func&& f)
            : function(std::forward<Func>(f)) {}

        Return call(Args&&...args) const override { 
            return function(std::forward<Args>(args)...); 
        };
    };

    template<class, class>
    class member_function_storage;
    template<class Object, class Return, class ...Args>
    class member_function_storage<Object, Return(Args...)> : public function_storage<Return, Args...> {
        Return(Object::* function)(Args...);
        Object& obj;

    public:
        member_function_storage(Return(Object::* function)(Args...), Object& obj)
            : function(function), obj(obj) {}

        Return call(Args&&...args) const override { 
            return (obj.*function)(std::forward<Args>(args)...); 
        };
    };

    template<class T>
    class function;
    template<class Return, class ...Args>
    class function<Return(Args...)> {
    public:
        using result_type = Return;
        using argument_types = std::tuple<Args...>;

        function() = default;

        template<class Type>
        function(result_type(Type::* a)(Args...), Type& t)
            : storage(new member_function_storage<Type, result_type(Args...)>{ a, t }) {}

        template<std::invocable<Args...> Func>
        function(Func&& t)
            : storage(new typed_function_storage<Func, result_type(Args...)>{ std::forward<Func>(t) }) {}

        function(const function& f)
            : storage(f.storage) { if (storage) storage->ref_count++; }

        function(function&& f)
            : storage(f.storage) { f.storage = nullptr; }

        ~function() { clean(); }

        function& operator=(const function& f) {
            clean();
            storage = f.storage;
            if (storage)
                storage->ref_count++;
            return *this;
        }

        function& operator=(function&& f) {
            clean();
            storage = f.storage;
            f.storage = nullptr;
            return *this;
        }

        inline result_type operator()(Args ...args) const {
            return storage->call(std::forward<Args>(args)...);
        }

        inline operator bool() const { return storage; }

    private:
        function_storage<result_type, Args...>* storage = nullptr;
        void clean() { 
            if (storage) {
                storage->ref_count--;
                if (storage->ref_count == 0) 
                    delete storage;
            }
        }
    };

	template<class>
    class pa_function;
	template<class Return, class...Args>
	class pa_function<Return(Args...)> {
    public:
        using result_type = Return;
        using argument_types = std::tuple<Args...>;

        template<class ...Tys> requires std::constructible_from<function<result_type(Args...)>, Tys...>
        pa_function(Tys&&...f)
            : function(std::forward<Tys>(f)...) {}

        pa_function(pa_function&& f)
            : function(std::move(f.function)) {}

        pa_function(const pa_function& f)
            : function(f.function) {}

        auto& operator=(pa_function&& f) {
            function = std::move(f.function);
            return *this;
        }

        auto& operator=(const pa_function& f) {
            function = f.function;
            return *this;
        }

        template<class ...Tys> requires (are_first_n<result_type(Args...), Tys...> && sizeof...(Tys) < sizeof...(Args) && sizeof...(Tys) > 0)
        inline pa_function<typename last_n_args<result_type(Args...), sizeof...(Args) - sizeof...(Tys)>::type> operator()(Tys&&...args) const {
            auto function = this->function;
            return [function, ...args = std::forward<Tys>(args)] (auto...rest) -> result_type {
                return function(args..., std::forward<decltype(rest)>(rest)...); };
        }

        inline result_type operator()(Args ...args) const { return function(std::forward<Args>(args)...); }

        inline operator bool() const { return function; }

    private:
		function<result_type(Args...)> function;
	};

    template <class Return, class ...Args>
    function(Return(Args...))->function<Return(Args...)>;

    template <class Return, class T, class ...Args>
    function(Return(T::* a)(Args...), T&)->function<Return(Args...)>;

    template <class _Fx>
    function(_Fx)->function<typename lambda_signature<_Fx>::type>;

	template <class Return, class ...Args>
	pa_function(Return(Args...))->pa_function<Return(Args...)>;

	template <class Return, class T, class ...Args>
	pa_function(Return(T::* a)(Args...), T&)->pa_function<Return(Args...)>;

	template <class _Fx>
	pa_function(_Fx)->pa_function<typename lambda_signature<_Fx>::type>;
}