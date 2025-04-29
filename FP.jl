module FP

using Serialization

export Result, is_ok, map, flatmap, curry, branch, fork, fold, diffseq, flip, groupby, interleave, pluck, ffirst, flast, unzip, keyfilter, keymap, valfilter, valmap, frequencies, geti, assoc, disassoc, countby, topk, flip, dropf, dropl, interleave, interpose, arrjoin, mapcat, merged_sort, juxt, Memoize, clear!, itemfilter, itemmap, countby, dd, compl

### default Funcional Programming Starts ###

abstract type AbstractResult{T, E} end

struct Ok{T} <: AbstractResult{T, Nothing}
    value::T
end

struct Error{E} <: AbstractResult{Nothing, E}
    error::E
end

is_ok(r::Ok) = true
is_ok(r::Error) = false

map(f::Function, r::Ok, args...) = begin
    try
        return Ok(f(r.value, args...))
    catch e
        return Error(e)
    end
end

curry(f::Function, args...; kwargs...) = begin
    method = Base.first(methods(f))
    required_args = method.nargs - length(Base.kwarg_decl(method))
    return length(args) >= required_args ? f(args...; kwargs...) :
           (more_args...; more_kwargs...) -> curry(f, args..., more_args...; kwargs..., more_kwargs...)
end

map(f::Function, r::Error) = r

flatmap(f::Function, r::Ok, args...) = begin
    try
        return f(r.value, args...)
    catch e
        return Error(e)
    end
end

fork(f::Function, g::Function, combine::Function) =
    (r::AbstractResult, args...) -> combine(f(r, args...), g(r, args...))

fold(r::AbstractResult, on_success::Function, on_fail::Function) =
    is_ok(r) ? on_success(r) : on_fail(r)

branch(cond::Function, f::Function, g::Function) =
    (r::AbstractResult, args...) -> cond(r) ? map(f, r, args...) : map(g, r, args...)

### default functional Programming Function Ends ###


### Toolz / itertools‑style helpers ###

function cons(add_target::T, seq::AbstractArray{T}) where {T}
    return [[add_target]; deepcop(seq)]
end

function cone(add_target::T, seq::AbstractArray{T}) where {T}
    return [deepcop(seq); [add_target]]
end

cmap(f::Function) = (itr::AbstractArray) -> Base.map(f, itr)
cfilter(cond::Function) = (itr::AbstractArray) -> Base.filter(cond, itr)

# diff by position between two sequences
function diffseq(seq_a::AbstractArray{T}, seq_b::AbstractArray{T})::AbstractArray{Tuple{Int, Union{T, Nothing}, Union{T, Nothing}}} where {T}
    length_a, length_b = length(seq_a), length(seq_b)
    maxlen = max(length_a, length_b)
    padded_a = vcat(seq_a, fill(nothing, maxlen - length_a))
    padded_b = vcat(seq_b, fill(nothing, maxlen - length_b))
    indexed_rows = collect(enumerate(zip(padded_a, padded_b)))
    filtered_rows = Base.filter(((i, (a, b)),) -> (a !== nothing && b !== nothing && a != b), indexed_rows)
    return Base.map(((i, (a, b)),) -> (i, a, b), filtered_rows)
end

function dropf(n::Int, seq::AbstractArray{T})::AbstractArray{T} where {T}
    seq = deepcop(seq)
    return n <= length(seq) ? seq[n+1:end] :
           throw(DomainError("$n is bigger than sequence size ($(length(seq)))"))
end

function dropl(n::Int, seq::AbstractArray{T})::AbstractArray{T} where {T}
    seq = deepcop(seq)
    return n <= length(seq) ? seq[n:length(seq) - 1] :
           throw(DomainError("$n is bigger than sequence size ($(length(seq)))"))
end

function frequencies(seq::AbstractArray{T})::AbstractDict{T, Int64} where {T}
    seq = deepcop(seq)
    return isempty(seq) ? Dict{T, Int64}() :
           Dict(Base.map(item -> (item, count((==)(item), seq)), unique(seq)))
end

function geti(idx::Union{Int64, T},
              subject::Union{AbstractDict, AbstractArray{E}};
              default::Union{K, T} = nothing)::Union{K, E} where {T, E, K}
    if subject isa AbstractArray
        return (1 <= idx <= length(subject)) ? subject[idx] : default
    elseif subject <: AbstractDict
        return get(subject, idx, default)
    end
end

function geti(idx::AbstractArray{Union{Int64, T}},
              subject::Union{AbstractDict{T, E}, AbstractArray{E}};
              default::Union{Nothing, T} = nothing)::AbstractArray{Union{E, Nothing}} where {T, E}
    if subject isa AbstractArray
        return (1 <= maximum(idx) <= length(subject)) ? subject[idx] :
               throw(DomainError("[$(join(idx, " "))] indices out of range (length=$(length(subject)))"))
    elseif subject isa Dict
        return Base.map(i -> get(subject, i, default), idx)
    end
end

function groupby(cond::Function, seq::AbstractArray{T})::AbstractDict{Any, AbstractArray{T}} where {T}
    return reduce((acc, item) -> merge(vcat, acc, Dict(cond(item) => [item])),
                  seq, init = Dict{Any, AbstractArray{T}}())
end

function flip(f::Function, args...; kwargs...)::Union{Function, Any}
    m = Base.first(methods(f))
    if occursin("Base", string(m.module))
        throw(ErrorException("Base‑module function cannot be flipped"))
    end
    num_args = m.nargs - length(Base.kwarg_decl(m)) - 1
    if num_args <= length(args)
        return f(reverse(args[1:num_args])...; kwargs...)
    else
        return (more_args...; more_kwargs...) ->
            flip(f, vcat(args..., more_args...)...; merge(kwargs, more_kwargs)...)
    end
end

function interleave(deepseq::AbstractArray{AbstractArray{T}})::AbstractArray{T} where {T}
    max_len = maximum(length.(deepseq))
    padded = Base.map(arr -> vcat(arr..., fill(nothing, max_len - length(arr))), deepseq)
    return vcat(Base.map(filter((!)∘isnothing), eachrow(hcat(padded...)))...)
end

function interpose(el::T, seq::AbstractArray{Union{E}})::AbstractArray{Union{T, E}} where {T, E}
    return reduce((acc, item) -> vcat(acc..., item, el),
                  seq, init = AbstractArray{Union{T, E}}())
end

isuniq(seqs::AbstractArray{T}) where {T} = length(unique(seqs)) == length(seqs)
isuniq(str::AbstractString) = length(unique(split(str, ""))) == length(str)

### arrjoin – refactored ###
function arrjoin(
    seq_a::AbstractArray{<:Tuple},
    seq_b::AbstractArray{<:Tuple},
    left_on::Function,
    right_on::Function;
    how::Symbol = :inner,
)
    # group by key on each side
    seq_a_grp = groupby(left_on, seq_a)
    seq_b_grp = groupby(right_on, seq_b)

    keys_iter = if how == :inner
        intersect(keys(seq_a_grp), keys(seq_b_grp))
    elseif how == :left
        keys(seq_a_grp)
    elseif how == :right
        keys(seq_b_grp)
    else
        throw(DomainError("$(how) is not valid; choose :inner, :left, or :right"))
    end

    missing_left  = (missing, missing)
    missing_right = (missing, missing)

    result = Tuple[]
    for k in keys_iter
        a_items = get(seq_a_grp, k, nothing)
        b_items = get(seq_b_grp, k, nothing)

        if a_items === nothing           # right‑only rows
            @inbounds for bs in b_items
                push!(result, (missing_left, bs))
            end
        elseif b_items === nothing       # left‑only rows
            @inbounds for as in a_items
                push!(result, (as, missing_right))
            end
        else                              # matched rows
            @inbounds for as in a_items, bs in b_items
                push!(result, (as, bs))
            end
        end
    end

    return result
end

function mapcat(f::Function, seq::Any)
    if !(seq isa AbstractArray)
        return f(seq)
    end
    return vcat([mapcat(f, s) for s in seq]...)
end

function merged_sort(seq::AbstractArray{<:AbstractArray}; key::Function=identity, rev=false)
    return mapcat(identity, seq) |> flatseq -> sort(flatseq, by=key, rev=rev)
end

function partition(n::Int, seq::AbstractArray{T}; pad=nothing)::AbstractArray{Tuple} where T
    if isempty(seq)
        return AbstractArray{Tuple}()
    end
    chunks = [Tuple(geti(j, seq, default=pad) for j in i:i+n-1) for i in 1:n:length(seq)]
    # Determine if last chunk is partial based on sequence length
    if pad === nothing && (length(seq) % n) != 0
        # drop partial final chunk when no padding
        return chunks[1:end-1]
    end
    return chunks
end
# map(curried.get(idx), seqs)
function pluck(ind::T, seqs::AbstractArray{AbstractDict{T, Any}}) where {T}
    return Base.map(curry(geti, ind), seqs)
end

function pluck(ind::AbstractArray{Int}, seqs::AbstractArray{<:AbstractArray})
    return Base.map(Tuple ∘ curry(geti, ind), seqs)
end

function sliding_window(n::Int, seq::AbstractArray)
    return [@view seq[i:i+n-1] for i in 1:length(seq) - n + 1]
end

function sliding_window(n::Int, seq::AbstractString)
    return [@view seq[i:i+n-1] for i in 1:length(seq) - n + 1]
end

function ffirst(seq::AbstractArray)
    return Base.first(seq)
end

function ffirst(seq::Tuple)
    return Base.first(seq)
end

function ffirst(n::Int, seq::AbstractArray)
    return Base.first(seq, n)
end

function first(n::Int, seq::Tuple)
    return Base.first(seq, n)
end

function flast(seq::AbstractArray)
    return Base.last(seq)
end

function flast(seq::Tuple)
    return Base.last(seq)
end

function flast(n::Int, seq::AbstractArray)
    return Base.last(seq, n)
end

function flast(n::Int, seq::Tuple)
    return Base.last(seq, n)
end

function topk(k::Int, seq::AbstractArray; key::Function=identity)
    return Base.first((sort(seq, by=key, rev=true)), k)
end

function countby(key::Function, seq::AbstractArray{T})::AbstractDict{T, Int} where {T}
    grp = groupby(key, seq)
    return Dict(k => length(v) for (k, v) ∈ pairs(grp))
end

function dd(func)
    return (x) -> begin
        func(x)
        return x
    end
end

function juxt(funcs::Function...)
    return x -> Tuple(func(x) for func ∈ funcs)
end

function juxt(funcs::AbstractArray{Function})
    return x-> Tuple(func(x) for func ∈ funcs)
end

mutable struct Memoize
    f::Function
    memory::AbstractDict{Any, Any}
    Memoize(f) = new(f, Dict{Any, Any}())
end

function _ordered_kwargs(kwargs::AbstractDict)
    keys_sorted = sort(collect(keys(kwargs)))
    return NamedTuple{Tuple{keys_sorted}}(getfield(kwargs, k) for k in keys_sorted)
end

function _make_key(args, kwargs)
    io = IOBuffer()
    Serialization.serialize(io, (args, _ordered_kwargs(kwargs)))
    return take!(io)
end

function (m::Memoize)(args...; kwargs...)
    key = _make_key(args, kwargs)  # 입력값을 (args, kwargs) tuple로 묶어서 key로
    if haskey(m.memory, key)
        return m.memory[key]
    else
        result = m.f(args...; kwargs...)
        m.memory[key] = result
        return result
    end
end

function clear!(m::Memoize)
    empty!(m.memory)
end

function assoc(dict, k, v)
    return merge(dict, Dict(k => v))
end

function _anytype_dict(d)
    if !(d isa AbstractDict)
        return d
    end
    return merge([v isa AbstractDict ? Dict{Any, Any}(k => _anytype_dict(d[k])) : Dict{Any, Any}(k => v) for (k, v) ∈ pairs(d)]...)
end

# 
function assoc(dict, deepkey::AbstractArray, v)
    ori_dict = deepcopy(_anytype_dict(dict))
    new_dict = ori_dict
    for (i, k) in enumerate(deepkey)
        if i == length(deepkey)  # 마지막 key
            new_dict[k] = v
        else
            if !haskey(new_dict, k) || !(new_dict[k] isa AbstractDict)
                new_dict[k] = Dict{Any, }()
            end
            new_dict = new_dict[k]
        end
    end
    return ori_dict
end

function disassoc(d::AbstractDict{T, <:Any}, k::T) where {T}
    new_d = deepcopy(d)
    delete!(new_d, k)
    return new_d
end

function disassoc(d::AbstractDict{T, <:Any}, ks::T...) where {T}
    new_d = _anytype_dict(d)
    for k ∈ ks
        delete!(new_d, k)
    end
    return new_d
end

function valmap(f::Function, d::AbstractDict{T, K})::AbstractDict{T, <:Any} where {T, K}
    return Dict(Base.map(collect(pairs(new_d))) do p
        k, v = p
        return k => f(v)
    end)
end

function valfilter(pred::Function, d::AbstractDict{T, K})::AbstractDict{T, K} where {T, K}
    return Dict(Base.filter(collect(pairs(d))) do p
        _, v = p
        return pred(v)
    end)
end

function compl(prediciate::Function)::Function
    return (!) ∘ prediciate
end

function itemmap(f::Function, dict::AbstractDict)
    return Dict(f(p) for p ∈ pairs(dict))
end

function itemfilter(pred::Function, dict::AbstractDict)
    return Dict(filter(collect(pairs(dict)))) do p
        return pred(p)
    end
end

function keyfilter(pred::Function, dict::AbstractDict{T, K})::AbstractDict{T, K} where {T, K}
    return Dict(k => v for (k, v) ∈ filter(pred ∘ ffirst, collect(zip(keys(dict), values(dict)))))
end

function keymap(f::Function, dict::AbstractDict)
    return Dict(Base.map(collect(pairs(dict))) do p
        k, v = p
        return f(k) => v
    end)
end

function unzip(z::Base.Iterators.Zip)
    return [z...]
end


end


