(symbols 'unit 'pico)

# Ensure 'put does not modify NIL!
(redef put @
  (unless (arg 1)
    (quit "put attempted to modify NIL!"))
  (pass put))

(de num-to-fp (Num)
  (format (pack Num ".") *Scl))

# https://rosettacode.org/wiki/Deepcopy#PicoLisp

# non-cyclic deep copy
(de dcp (X)
  (if (atom X)
    X
    (cons (deepCopy (car X)) (deepCopy (cdr X))) ) )

# cyclic deep copy
(de dcpc (X)
  (let Mark NIL
    (recur (X)
        (cond
          ((atom X) X)
          ((asoq X Mark) (cdr @))
          (T
              (prog1 (cons)
                (push 'Mark (cons X @))
                (set @ (recurse (car X)))
                (con @ (recurse (cdr X))) ) ) ) ) ) )

# acts like flip:
# (setq X (rotr X))
(de rotl (Lst Cnt)
  (default Cnt T)

  # move first cell to end:
  # set last cell cdr to first cell
  # set first cell cdr to NIL

  (let (Car Lst
        Cdr (cdr Lst)
        Last
          # loop until Cnt or atom
          (for ((I . Lst) Lst (and (pair Lst) (<= I Cnt)) (cdr Lst)) Lst))
    (con Car (cdr Last))
    (con Last Car)
    Cdr))


(de expose "Keys"
  #{

    (expose
      (key
        meth...)
      (key
        meth...))

    only needs to be called once?

    don't if the method exists

  }#
  (let (Defs (val (car "Keys"))
        Prog
          (make
            (link
              (list 'extend (car "Keys")))
            (for Key (cdr "Keys")
              (let Cls (car (type (get This Key)))
                # watch for cls vars?
                # skip T
                (for Meth (val Cls)
                  (let ((Nm Args) Meth)
                    (unless (or (= Nm T) (get Defs Nm))
                      (link
                        (list 'dm Nm '@
                          (list 'pass
                            (cons 'quote Nm)
                            (list ': Key)))))))))))
    (pretty Prog) (prinl)
    (run Prog)
    (setq expose '(()))))

(de sha256 (Str)
  (mapcar
    '((B) (pad 2 (hex B)))
    (native
      "libcrypto.so"
      "SHA256"
      '(B . 32)
      Str
      (length Str)
      '(NIL (32)) ) ) )

(de hash64 (X)
  #{

    https://gist.github.com/Seteeri/351e66f3c41353041f48cb2616a7be97

    # https://stackoverflow.com/questions/
    # 6943493/hash-table-with-64-bit-values-as-key

    public long hash64shift(long key)
    {
      key = (~key) + (key << 21); // key = (key << 21) - key - 1;
      key = key ^ (key >>> 24);
      key = (key + (key << 3)) + (key << 8); // key * 265
      key = key ^ (key >>> 14);
      key = (key + (key << 2)) + (key << 4); // key * 21
      key = key ^ (key >>> 28);
      key = key + (key << 31);
      return key;
    }

    also:

    # Thomas Wang's 64 bit Mix Function:
    # http://www.cris.com/~Ttwang/tech/inthash.htm
    inline unsigned intHash(uint64_t key)
    {
        key += ~(key << 32);
        key ^= (key >> 22);
        key += ~(key << 13);
        key ^= (key >> 8);
        key += (key << 3);
        key ^= (key >> 15);
        key += ~(key << 27);
        key ^= (key >> 31);
        return static_cast<unsigned>(key);
    }

  }#

  # get n bits from X
  (setq
    X
      (+
        (bin
          (pack
            (make
              (for B (chop (bin X))
                (link (if (= B "1") "0" "1"))))))
        (>> X -21))
    X (^ X (>> X 24))
    X (+ X (>> X -3) (>> X -8))
    X (^ X (>> X 14))
    X (+ X (>> X -4) (>> X -2))
    X (^ X (>> X 28))
    X (+ X (>> X -31))))
