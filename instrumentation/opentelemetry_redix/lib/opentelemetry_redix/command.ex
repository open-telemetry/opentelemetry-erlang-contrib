defmodule OpentelemetryRedix.Command do
  @moduledoc false

  @doc """
  Masks potentially sensitive information in Redis commands.
  """
  def sanitize([name | args] = command) do
    case strategy(name) do
      :all -> Enum.join(command, " ")
      {:keep, n} -> keep_args(name, args, n)
      {:keyval, n} -> keep_args_keyval(name, args, n)
    end
  end

  defp keep_args(cmd, [], _), do: cmd
  defp keep_args(cmd, [_ | xs], 0), do: keep_args(cmd <> " ?", xs, 0)
  defp keep_args(cmd, [x | xs], n), do: keep_args(cmd <> " #{x}", xs, n - 1)

  defp keep_args_keyval(cmd, [], _), do: cmd
  defp keep_args_keyval(cmd, [_], 0), do: cmd
  defp keep_args_keyval(cmd, [k, _ | kvs], 0), do: keep_args_keyval(cmd <> " #{k} ?", kvs, 0)
  defp keep_args_keyval(cmd, [x | xs], n), do: keep_args_keyval(cmd <> " #{x}", xs, n - 1)

  # Cluster
  defp strategy(cmd) when cmd in ~w(CLUSTER READONLY READWRITE), do: :all

  # Connection
  defp strategy(cmd) when cmd in ~w(AUTH), do: {:keep, 0}
  defp strategy(cmd) when cmd in ~w(HELLO), do: {:keep, 2}
  defp strategy(cmd) when cmd in ~w(CLIENT ECHO PING QUIT SELECT), do: :all

  # Geo
  defp strategy(cmd) when cmd in ~w(GEOADD GEODIST GEOHASH GEOPOS GEORADIUS GEORADIUSBYMEMBER),
    do: :all

  # Hashes
  defp strategy(cmd) when cmd in ~w(HMSET HSET), do: {:keyval, 1}
  defp strategy(cmd) when cmd in ~w(HSETNX), do: {:keep, 2}

  defp strategy(cmd) when cmd in ~w(
    DEL
    HEXISTS
    HGET
    HGETALL
    HINCRBY
    HINCRBYFLOAT
    HKEYS
    HLEN
    HMGET
    HSCAN
    HSTRLEN
    HVAL
  ),
    do: :all

  # HyperLogLog
  defp strategy(cmd) when cmd in ~w(PFADD), do: {:keep, 1}
  defp strategy(cmd) when cmd in ~w(PFCOUNT PFMERGE), do: :all

  # Keys
  # MIGRATE can contain AUTH data
  defp strategy(cmd) when cmd in ~w(MIGRATE), do: {:keep, 6}
  defp strategy(cmd) when cmd in ~w(RESTORE), do: {:keep, 2}

  defp strategy(cmd) when cmd in ~w(
    DEL
    DUMP
    EXISTS
    EXPIRE
    EXPIREAT
    KEYS
    MOVE
    OBJECT
    PERSIST
    PEXPIRE
    PEXPIREAT
    PTTL
    RANDOMKEY
    RENAME
    RENAMENX
    SCAN
    SORT
    TOUCH
    TTL
    TYPE
    UNLINK
    WAIT
  ),
    do: :all

  # Lists
  defp strategy(cmd) when cmd in ~w(LINSERT), do: {:keep, 2}
  defp strategy(cmd) when cmd in ~w(LPOS LPUSH LPUSHX LREM LSET RPUSH RPUSHX), do: {:keep, 1}

  defp strategy(cmd) when cmd in ~w(
    BLMOVE
    BLPOP
    BRPOP
    BRPOPLPUSH
    LINDEX
    LLEN
    LMOVE
    LPOP
    LRANGE
    LTRIM
    RPOP
    RPOPLPUSH
  ),
    do: :all

  # Pub/Sub
  defp strategy(cmd) when cmd in ~w(PUBLISH), do: {:keep, 1}

  defp strategy(cmd) when cmd in ~w(PSUBSCRIBE PUBSUB PUNSUBSCRIBE SUBSCRIBE UNSUBSCRIBE),
    do: :all

  # Server
  # CONFIG SET can set any property, including the master password
  defp strategy(cmd) when cmd in ~w(CONFIG), do: {:keep, 2}

  defp strategy(cmd) when cmd in ~w(
    ACL
    BGREWRITEAOF
    BGSAVE
    COMMAND
    DBSIZE
    DEBUG
    FLUSHALL
    FLUSHDB
    INFO
    LASTSAVE
    LATENCY
    LOLWUT
    MEMORY
    MODULE
    MONITOR
    PSYNC
    REPLICAOF
    ROLE
    SAVE
    SHUTDOWN
    SLAVEOF
    SLOWLOG
    SWAPDB
    SYNC
    TIME
  ),
    do: :all

  # Sets
  defp strategy(cmd) when cmd in ~w(SADD SISMEMBER SMISMEMBER SREM), do: {:keep, 1}
  defp strategy(cmd) when cmd in ~w(SMOVE), do: {:keep, 1}

  defp strategy(cmd) when cmd in ~w(
    SCARD
    SDIFF
    SDIFFSTORE
    SINTER
    SINTERSTORE
    SMEMBERS
    SPOP
    SRANDMEMBER
    SSCAN
    SUNION
    SUNIONSTORE
  ),
    do: :all

  # Sorted Sets
  defp strategy(cmd) when cmd in ~w(
    ZADD
    ZCOUNT
    ZINCRBY
    ZLEXCOUNT
    ZMSCORE
    ZRANGEBYLEX
    ZRANGEBYSCORE
    ZRANK
    ZREM
    ZREMRANGEBYLEX
    ZREMRANGEBYSCORE
    ZREVRANGEBYLEX
    ZREVRANGEBYSCORE
    ZREVRANK
    ZSCORE
  ),
    do: {:keep, 1}

  defp strategy(cmd) when cmd in ~w(
    BZPOPMAX
    BZPOPMIN
    ZCARD
    ZINTER
    ZINTERSTORE
    ZPOPMAX
    ZPOPMIN
    ZRANGE
    ZREMRANGEBYRANK
    ZREVRANGE
    ZSCAN
    ZUNION
    ZUNIONSTORE
  ),
    do: :all

  # Streams
  defp strategy(cmd) when cmd in ~w(XADD), do: {:keyval, 2}

  defp strategy(cmd) when cmd in ~w(
    XACK
    XCLAIM
    XDEL
    XGROUP
    XINFO
    XLEN
    XPENDING
    XRANGE
    XREAD
    XREADGROUP
    XREVRANGE
    XTRIM
  ),
    do: :all

  # Strings
  defp strategy(cmd) when cmd in ~w(APPEND GETSET SET SETNX SETRANGE), do: {:keep, 1}
  defp strategy(cmd) when cmd in ~w(PSETEX SETEX), do: {:keep, 2}
  defp strategy(cmd) when cmd in ~w(MSET MSETNX), do: {:keyval, 0}

  defp strategy(cmd) when cmd in ~w(
    BITCOUNT
    BITFIELD
    BITOP
    BITPOS
    DECR
    DECRBY
    GET
    GETBIT
    GETRANGE
    INCR
    INCRBY
    INCRBYFLOAT
    MGET
    SETBIT
    STRALGO
    STRLEN
  ),
    do: :all

  # Transactions
  defp strategy(cmd) when cmd in ~w(DISCARD EXEC MULTI UNWATCH WATCH), do: :all

  # Default
  defp strategy(_cmd), do: {:keep, 0}
end
