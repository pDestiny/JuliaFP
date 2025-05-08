module FP

using Serialization

export AbstractResult, Ok, Err, is_ok, map, flatmap, curry, branch, fork, fold, diffseq, flip, groupby, interleave, pluck, ffirst, flast, unzip, keyfilter, keymap, valfilter, valmap, frequencies, geti, assoc, disassoc, countby, topk, flip, dropf, dropl, interleave, interpose, arrjoin, mapcat, merged_sort, juxt, Memoize, clear!, itemfilter, itemmap, countby, dd, compl, fork

### default Funcional Programming Starts ###

abstract type AbstractResult{T, E} end

struct Ok{T} <: AbstractResult{T, Nothing}
    value::T
    error::Nothing
end

struct Err{E} <: AbstractResult{Nothing, E}
    value::Nothing
    error::E
    backtrace::AbstractArray
end

Ok(value::T) where T = Ok(value, nothing)
Err(error::E, backtrace::AbstractArray) where E = Err(nothing, error, backtrace)
Err(error::E) where E = Err(nothing, error, [])

is_ok(r::Ok) = true
is_ok(r::Err) = false

mmap(f::Function, r::AbstractResult) = begin
    try
        return r isa Err ? r : Ok(f(r.value))
    catch e
        bt = catch_backtrace()
        return Err(e, bt)
    end
end

cmmap(f::Function) = (r::AbstractResult) -> mmap(f, r)



flatmmap(f::Function, r::AbstractResult) = begin
    try
        return r isa Err ? r : f(r.value)
    catch e
        return Err(e, catch_backtrace())
    end
end

cflatmmap(f::Function) = (r::AbstractResult) -> flatmmap(f, r)

function fork(combfunc::Function, f::Function, g::Function)
    return (r::AbstractResult) -> begin
        try
            fr = mmap(f, r)
            gr = mmap(g, r)
            if is_ok(fr) && is_ok(gr)
                return Ok(combfunc(fr.value, gr.value))
            else
                if !is_ok(fr)
                    return fr
                end

                if !is_ok(gr)
                    return gr
                end                
            end
        catch e
            return Err(e, catch_backtrace())
        end
    end
end

fold(on_success::Function, on_fail::Function) = (r::AbstractResult) ->
    is_ok(r) ? on_success(r) : on_fail(r)

branch(cond::Function, f::Function, g::Function) =
    (r::AbstractResult) -> begin
        condition = mmap(cond, r)
        if !is_ok(condition)
            return condition
        else
            if condition.value isa Bool
                return condition.value ? mmap(f, r) : mmap(g, r)
            else
                return Err(TypeError(:branch, "branch's condition function has returned an unexpected Type", Bool, typeof(condition.value)))
            end
        end
end
### default functional Programming Function Ends ###


#### Base Module FP wrapping Starts #####
cmap(f::Function) = (itr::Union{AbstractArray, AbstractUnitRange}) -> Base.map(f, itr)
cfilter(cond::Function) = (itr::Union{AbstractArray, AbstractUnitRange}) -> Base.filter(cond, itr)

# unified csplit definition
function csplit(spliter::AbstractString)
  return function(str::AbstractString)
    if typeof(str) <: SubString
      return Base.split(string(str), spliter)
    else
      return Base.split(str, spliter)
    end
  end
end

csplit(spliter::AbstractChar) = csplit(string(spliter))

function cjoin(delim::AbstractString)
  return function(seq::Union{AbstractArray, Base.AbstractVecOrTuple})
    if eltype(seq) <: SubString
      return Base.join(string.(seq), delim)
    else
      return Base.join(seq, delim)
    end
  end
end
cjoin(delim::AbstractChar) = cjoin("$delim")

function creplace(ps::Pair...)
    return function(str::AbstractString)
        if str isa SubString
            return Base.replace(string(str), ps...)
        else
            return Base.replace(str, ps...)
        end
    end
end
cstartswith(prefix::Union{AbstractString, Regex}) = (str::AbstractString) -> startswith(str, prefix)
cendswith(suffix::Union{AbstractString, Regex}) = (str::AbstractString) -> endswith(str, suffix)

#### Base Moudle FP wrapping Ends ####

### Toolz / itertools‑style helpers ###

curry(f::Function, args...; kwargs...) = begin
    method = Base.first(methods(f))
    if occursin("Base", string(method.module))
        required_args = method.nargs
    else
        required_args = method.nargs - 1
    end
    return length(args) >= required_args ? f(args...; kwargs...) :
           (more_args...; more_kwargs...) -> curry(f, args..., more_args...; kwargs..., more_kwargs...)
end

function cons(add_target::T, seq::AbstractArray{T}) where {T}
    return vcat(add_target, seq...)
end

cons(add_target::Union{SubString, AbstractString}, seq::AbstractString) = add_target * seq


function cone(add_target::T, seq::AbstractArray{T}) where {T}
    return vcat(seq..., add_target)
end

cone(add_target::Union{SubString, AbstractString}, seq::AbstractString) = seq * add_target



# diff by position between two sequences
function diffseq(a::AbstractVector{T}, b::AbstractVector{T}) where T
    diffs = Tuple{Int, Union{T, Nothing}, Union{T, Nothing}}[]
    maxlen = max(length(a), length(b))
    for i in 1:maxlen
        x = i <= length(a) ? a[i] : nothing
        y = i <= length(b) ? b[i] : nothing
        if x !== nothing && y !== nothing && x != y
            push!(diffs, (i, x, y))
        end
    end
    return diffs
end

"""
dropf : drop first n elements from a array. this function doesn't change original sequence
"""
function dropf(n::Int, seq::AbstractArray{T})::AbstractArray{T} where {T}
    return n <= length(seq) ? vcat(seq[n+1:end]...) :
           throw(DomainError("$n is bigger than sequence size ($(length(seq)))"))
end

"""
dropl : drop last n elements from a array. this function doesn't change original sequence
"""
function dropl(n::Int, seq::AbstractArray{T})::AbstractArray{T} where {T}
    return n <= length(seq) ? vcat(seq[1:length(seq) - n]...) :
        throw(DomainError("$n is bigger than sequence size ($(length(seq)))"))
end

"""
frequencies : count unique items in a array and return dictionary with count as value for each unique item
"""
function frequencies(seq::AbstractArray{T})::Dict{T, Int} where T
    freq = Dict{T, Int}()
    for item in seq
        freq[item] = Base.get(freq, item, 0) + 1
    end
    return freq
end

"""
geti : idx 값을 받아, subject(array) 의 index 값을 반환하는 함수
"""
function geti(idx::Int,
              subject::AbstractArray{T};
              default::Union{Nothing, T} = nothing)::T where {T}
    try
        return subject[idx]
    catch
        if isnothing(default)
            throw(BoundsError(subject, idx))
        else
            return default
        end
    end
end

"""
geti : idx 값을 받아, subject(dictionary) 의 값을 반환하는 함수
"""
function geti(idx::T, subject::AbstractDict{T, K}; default::Union{Nothing, K}=nothing)::Union{Nothing, K} where {T, K}
    return Base.get(subject, idx, default)
end

"""
geti : idx array 를 받아 index 값들에 대한 element 값을 array에서 가져오는 함수
"""
function geti(idx::AbstractArray{Int}, subject::AbstractArray{T})::AbstractArray{T} where {T}
    return subject[idx]
end

"""
geti : dictionary 의 여러 level 깊이의 값을 가져온다.
"""
function geti(idx::AbstractArray{T}, subject::AbstractDict) where {T}
    new_dict::Dict{Any, Any} = deepcopy(subject)
    for (i, k) ∈ enumerate(idx)
        if i == length(idx)
            return new_dict[k]
        else
            new_dict = new_dict[k]
        end
    end
end

function groupby(cond::Function, seq::AbstractArray{T}) where {T}
    result = Dict{Any, Vector{T}}()
    for item in seq
      key = cond(item)
      if haskey(result, key)
        push!(result[key], item)
      else
        result[key] = [item]
      end
    end
    return result
end

function flip(f::Function, args...; kwargs...)
    m = Base.first(methods(f))
    if occursin("Base", string(m.module))
        throw(ErrorException("Base‑module function cannot be flipped"))
    end
    num_args = m.nargs - 1
    if num_args <= length(args)
        return f(reverse(args[1:num_args])...; kwargs...)
    else
        return (more_args...; more_kwargs...) ->
            flip(f, args..., more_args...; kwargs..., more_kwargs...)
    end
end

function interleave(seqs::AbstractArray)
    result = Any[]
    maxlen = isempty(seqs) ? 0 : maximum(length.(seqs))
    for i in 1:maxlen
        for s in seqs
            if i <= length(s)
                push!(result, s[i])
            end
        end
    end
    return result
end

function interpose(el::T, seq::AbstractArray{Union{E}})::AbstractArray{Union{T, E}} where {T, E}
    elems = Vector{T}()
    for (i, item) ∈ enumerate(seq)
        if i == length(seq)
            push!(elems, item)
        else
            push!(elems, item)
            push!(elems, el)
        end
    end
    return elems
end

function interpose(el::Union{AbstractChar, AbstractString}, seq::AbstractString)::String
    return seq |> csplit("") |> cjoin(el)
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

    raw_keys = if how == :inner
        intersect(keys(seq_a_grp), keys(seq_b_grp))
    elseif how == :left
        keys(seq_a_grp)
    elseif how == :right
        keys(seq_b_grp)
    else
        throw(DomainError("$(how) is not valid; choose :inner, :left, or :right"))
    end
    keys_iter = sort(collect(raw_keys))

    missing_left  = (nothing, nothing)
    missing_right = (nothing, nothing)

    result = Any[]
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

function partition(n::Int, seq::AbstractArray{T}; pad=nothing)::Vector{Tuple} where T
    if isempty(seq)
        return Tuple[]
    end
    if pad === nothing
        # only full chunks
        maxstart = length(seq) - n + 1
        if maxstart < 1
            return Tuple[]
        end
        starts = collect(1:n:maxstart)
    else
        starts = collect(1:n:length(seq))
    end
    chunks = [Tuple(geti(j, seq; default=pad) for j in i:i+n-1) for i in starts]
    return chunks
end
# map(curried.get(idx), seqs)
function pluck(ind::T, seqs::AbstractArray{<:AbstractDict{T, K}}) where {T, K}
    return Base.map(curry(geti, ind, default=nothing), seqs)
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

function countby(key::Function, seq::AbstractArray)::Dict
    grp = groupby(key, seq)
    return Dict(k => length(v) for (k, v) ∈ pairs(grp))
end

function countby(key::Function, seq::AbstractString)::Dict
    return seq |> csplit("") |> curry(groupby, key) |> curry(valmap, Base.length)
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
    keys_tuple = Tuple(keys_sorted)
    return NamedTuple{keys_tuple}(Tuple(getfield(kwargs, k) for k in keys_sorted))
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

function valmap(f::Function, d::AbstractDict{T, K})::Dict{T, Any} where {T, K}
    return Dict(k => f(v) for (k, v) in pairs(d))
end

function valfilter(pred::Function, d::AbstractDict{T, K})::Dict{T, K} where {T, K}
    return Dict(k => v for (k, v) in pairs(d) if pred(v))
end

function compl(prediciate::Function)::Function
    return (!) ∘ prediciate
end

function itemmap(f::Function, dict::AbstractDict)
    return Dict(f(p) for p ∈ pairs(dict))
end

function itemfilter(pred::Function, dict::AbstractDict{T, K})::Dict{T, K} where {T, K}
    return Dict(k => v for (k, v) in pairs(dict) if pred((k, v)))
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


