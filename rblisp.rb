$kLPar = '('
$kRPar = ')'
$kQuote = "'"

class Nil
end
$kNil = Nil.new()

class Num
  def initialize(n)
    @data = n
  end
  attr_reader :data
end

class Sym
  def initialize(s)
    @data = s
  end
  attr_reader :data
end

class Error
  def initialize(s)
    @data = s
  end
  attr_reader :data
end

class Cons
  def initialize(a, d)
    @car = a
    @cdr = d
  end
  attr_accessor :car, :cdr
end

class Subr
  def initialize(fn)
    @data = fn
  end
  attr_reader :data
end

class Expr
  def initialize(a, b, e)
    @args = a
    @body = b
    @env = e
  end
  attr_reader :args, :body, :env
end

def safeCar(obj)
  if obj.class == Cons then
    return obj.car
  end
  return $kNil
end

def safeCdr(obj)
  if obj.class == Cons then
    return obj.cdr
  end
  return $kNil
end

$sym_table = {}
def makeSym(str)
  if str == 'nil' then
    return $kNil
  elsif not $sym_table.key?(str) then
    $sym_table[str] = Sym.new(str)
  end
  return $sym_table[str]
end
$sym_t = makeSym('t')
$sym_quote = makeSym('quote')
$sym_if = makeSym('if')
$sym_lambda = makeSym('lambda')
$sym_defun = makeSym('defun')
$sym_setq = makeSym('setq')

def makeExpr(args, env)
  return Expr.new(safeCar(args), safeCdr(args), env)
end

def nreverse(lst)
  ret = $kNil
  while lst.class == Cons do
    tmp = lst.cdr
    lst.cdr = ret
    ret = lst
    lst = tmp
  end
  return ret
end

def pairlis(lst1, lst2)
  ret = $kNil
  while lst1.class == Cons and lst2.class == Cons do
    ret = Cons.new(Cons.new(lst1.car, lst2.car), ret)
    lst1 = lst1.cdr
    lst2 = lst2.cdr
  end
  return nreverse(ret)
end

def isDelimiter(c)
  return c == $kLPar || c == $kRPar || c == $kQuote || /^\s$/ =~ c
end

def skipSpaces(str)
  return str.lstrip
end

def makeNumOrSym(str)
  num = str.to_i
  if num.to_s == str
    return Num.new(num)
  end
  return makeSym(str)
end

def readAtom(str)
  nxt = ''
  for i in 0 .. str.length do
    if isDelimiter(str[i]) then
      nxt = str[i..-1]
      str = str[0..i-1]
      break
    end
  end
  return makeNumOrSym(str), nxt
end

def read(str)
  str = skipSpaces(str)
  if str == '' then
    return Error.new('empty input'), ''
  elsif str[0] == $kRPar then
    return Error.new(sprintf('invalid syntax: %s', str)), ''
  elsif str[0] == $kLPar then
    return readList(str[1..-1])
  elsif str[0] == $kQuote then
    elm, nxt = read(str[1..-1])
    return Cons.new($sym_quote, Cons.new(elm, $kNil)), nxt
  else
    return readAtom(str)
  end
end

def readList(str)
  ret = $kNil
  while true do
    str = skipSpaces(str)
    if str == '' then
      return Error.new('unfinished parenthesis'), ''
    elsif str[0] == $kRPar then
      break
    end
    elm, nxt = read(str)
    if elm.class == Error then
      return elm, ''
    end
    ret = Cons.new(elm, ret)
    str = nxt
  end
  return nreverse(ret), str[1..-1]
end

def printObj(obj)
  type = obj.class
  if type == Nil then
    return 'nil'
  elsif type == Num || type == Sym then
    return sprintf('%s', obj.data)
  elsif type == Error then
    return sprintf('<error: %s>', obj.data)
  elsif type == Cons then
    return printList(obj)
  elsif type == Subr then
    return '<subr>'
  elsif type == Expr then
    return '<expr>'
  end
end

def printList(obj)
  ret = ''
  first = true
  while obj.class == Cons do
    if first then
      first = false
    else
      ret += ' '
    end
    ret += printObj(obj.car)
    obj = obj.cdr
  end
  if obj.class == Nil then
    return sprintf('(%s)', ret)
  end
  return sprintf('(%s . %s)', ret, printObj(obj))
end

def findVar(sym, env)
  while env.class == Cons do
    alist = env.car
    while alist.class == Cons do
      if alist.car.car == sym
        return alist.car
      end
      alist = alist.cdr
    end
    env = env.cdr
  end
  return $kNil
end

$g_env = Cons.new($kNil, $kNil)

def addToEnv(sym, val, env)
  env.car = Cons.new(Cons.new(sym, val), env.car)
end

def eval1(obj, env)
  type = obj.class
  if type == Nil || type == Num || type == Error then
    return obj
  elsif type == Sym then
    bind = findVar(obj, env)
    if bind == $kNil then
      return Error.new(sprintf('%s has no value', obj.data))
    end
    return bind.cdr
  end

  op = safeCar(obj)
  args = safeCdr(obj)
  if op == $sym_quote then
    return safeCar(args)
  elsif op == $sym_if then
    if eval1(safeCar(args), env) == $kNil then
      return eval1(safeCar(safeCdr(safeCdr(args))), env)
    end
    return eval1(safeCar(safeCdr(args)), env)
  elsif op == $sym_lambda then
    return makeExpr(args, env)
  elsif op == $sym_defun then
    expr = makeExpr(safeCdr(args), env)
    sym = safeCar(args)
    addToEnv(sym, expr, $g_env)
    return sym
  elsif op == $sym_setq then
    val = eval1(safeCar(safeCdr(args)), env)
    sym = safeCar(args)
    bind = findVar(sym, env)
    if bind == $kNil then
      addToEnv(sym, val, $g_env)
    else
      bind.cdr = val
    end
    return val
  end
  return apply(eval1(op, env), evlis(args, env), env)
end

def evlis(lst, env)
  ret = $kNil
  while lst.class == Cons do
    elm = eval1(lst.car, env)
    if elm.class == Error then
      return elm
    end
    ret = Cons.new(elm, ret)
    lst = lst.cdr
  end
  return nreverse(ret)
end

def progn(body, env)
  ret = $kNil
  while body.class == Cons do
    ret = eval1(body.car, env)
    body = body.cdr
  end
  return ret
end

def apply(fn, args, env)
  if fn.class == Error then
    return fn
  elsif args.class == Error then
    return args
  elsif fn.class == Subr then
    return fn.data[args]
  elsif fn.class == Expr then
    return progn(fn.body, Cons.new(pairlis(fn.args, args), fn.env))
  end
  return Error.new(printObj(fn) + " is not function")
end

def subrCar(args)
  safeCar(safeCar(args))
end

def subrCdr(args)
  safeCdr(safeCar(args))
end

def subrCons(args)
  Cons.new(safeCar(args), safeCar(safeCdr(args)))
end

def subrEq(args)
  x = safeCar(args)
  y = safeCar(safeCdr(args))
  if x.class == Num and y.class == Num then
    if x.data == y.data then
      return $sym_t
    end
    return $kNil
  elsif x.equal?(y) then
    return $sym_t
  end
  return $kNil
end

def subrAtom(args)
  if safeCar(args).class == Cons then
    return $kNil
  end
  return $sym_t
end

def subrNumberp(args)
  if safeCar(args).class == Num then
    return $sym_t
  end
  return $kNil
end

def subrSymbolp(args)
  if safeCar(args).class == Sym then
    return $sym_t
  end
  return $kNil
end

def subrAddOrMul(fn, init_val)
  return lambda { |args|
    ret = init_val
    while args.class == Cons do
      if args.car.class != Num then
        return Error.new('wrong type')
      end
      ret = fn.call(ret, args.car.data)
      args = args.cdr
    end
    return Num.new(ret)
  }
end
$subrAdd = subrAddOrMul(lambda{ |x, y| x + y }, 0)
$subrMul = subrAddOrMul(lambda{ |x, y| x * y }, 1)

def subrSubOrDivOrMod(fn)
  return lambda { |args|
    x = safeCar(args)
    y = safeCar(safeCdr(args))
    if x.class != Num || y.class != Num then
      return Error.new('wrong type')
    end
    return Num.new(fn.call(x.data, y.data))
  }
end
$subrSub = subrSubOrDivOrMod(lambda{ |x, y| x - y })
$subrDiv = subrSubOrDivOrMod(lambda{ |x, y| x / y })
$subrMod = subrSubOrDivOrMod(lambda{ |x, y| x % y })

addToEnv(makeSym('car'), Subr.new(method(:subrCar)), $g_env)
addToEnv(makeSym('cdr'), Subr.new(method(:subrCdr)), $g_env)
addToEnv(makeSym('cons'), Subr.new(method(:subrCons)), $g_env)
addToEnv(makeSym('eq'), Subr.new(method(:subrEq)), $g_env)
addToEnv(makeSym('atom'), Subr.new(method(:subrAtom)), $g_env)
addToEnv(makeSym('numberp'), Subr.new(method(:subrNumberp)), $g_env)
addToEnv(makeSym('symbolp'), Subr.new(method(:subrSymbolp)), $g_env)
addToEnv(makeSym('+'), Subr.new($subrAdd), $g_env)
addToEnv(makeSym('*'), Subr.new($subrMul), $g_env)
addToEnv(makeSym('-'), Subr.new($subrSub), $g_env)
addToEnv(makeSym('/'), Subr.new($subrDiv), $g_env)
addToEnv(makeSym('mod'), Subr.new($subrMod), $g_env)
addToEnv($sym_t, $sym_t, $g_env)


while true
  print('> ')
  str = STDIN.gets
  break if not str
  exp, _ = read(str)
  puts printObj(eval1(exp, $g_env))
end
