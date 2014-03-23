$kLPar = '('
$kRPar = ')'
$kQuote = "'"
$kNil = { 'tag' => 'nil', 'data' => 'nil' }

def safeCar(obj)
  if obj['tag'] == 'cons' then
    return obj['car']
  end
  return $kNil
end

def safeCdr(obj)
  if obj['tag'] == 'cons' then
    return obj['cdr']
  end
  return $kNil
end

def makeError(str)
  return { 'tag' => 'error', 'data' => str }
end

$sym_table = {}
def makeSym(str)
  if str == 'nil' then
    return $kNil
  elsif not $sym_table.key?(str) then
    $sym_table[str] = { 'tag' => 'sym', 'data' => str }
  end
  return $sym_table[str]
end

def makeNum(num)
  return { 'tag' => 'num', 'data' => num }
end

def makeCons(a, d)
  return { 'tag' => 'cons', 'car' => a, 'cdr' => d }
end

def makeSubr(fn)
  return { 'tag' => 'subr', 'data' => fn }
end

def makeExpr(args, env)
  return {
    'tag' => 'expr',
    'args' => safeCar(args),
    'body' => safeCdr(args),
    'env' => env }
end

def nreverse(lst)
  ret = $kNil
  while lst['tag'] == 'cons' do
    tmp = lst['cdr']
    lst['cdr'] = ret
    ret = lst
    lst = tmp
  end
  return ret
end

def pairlis(lst1, lst2)
  ret = $kNil
  while lst1['tag'] == 'cons' and lst2['tag'] == 'cons' do
    ret = makeCons(makeCons(lst1['car'], lst2['car']), ret)
    lst1 = lst1['cdr']
    lst2 = lst2['cdr']
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
  if /^[+-]?\d+$/ =~ str then
    return makeNum(str.to_i)
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
    return makeError('empty input'), ''
  elsif str[0] == $kRPar then
    return makeError(sprintf('invalid syntax: %s', str)), ''
  elsif str[0] == $kLPar then
    return readList(str[1..-1])
  elsif str[0] == $kQuote then
    elm, nxt = read(str[1..-1])
    return makeCons(makeSym('quote'), makeCons(elm, $kNil)), nxt
  else
    return readAtom(str)
  end
end

def readList(str)
  ret = $kNil
  while true do
    str = skipSpaces(str)
    if str == '' then
      return makeError('unfinished parenthesis'), ''
    elsif str[0] == $kRPar then
      break
    end
    elm, nxt = read(str)
    if elm['tag'] == 'error' then
      return elm
    end
    ret = makeCons(elm, ret)
    str = nxt
  end
  return nreverse(ret), str[1..-1]
end

def printObj(obj)
  if obj['tag'] == 'num' || obj['tag'] == 'sym' || obj['tag'] == 'nil' then
    return sprintf('%s', obj['data'])
  elsif obj['tag'] == 'error' then
    return sprintf('<error: %s>', obj['data'])
  elsif obj['tag'] == 'cons' then
    return printList(obj)
  elsif obj['tag'] == 'subr' then
    return '<subr>'
  elsif obj['tag'] == 'expr' then
    return '<expr>'
  end
end

def printList(obj)
  ret = ''
  first = true
  while obj['tag'] == 'cons' do
    if first then
      ret = printObj(obj['car'])
      first = false
    else
      ret += ' ' + printObj(obj['car'])
    end
    obj = obj['cdr']
  end
  if obj['tag'] == 'nil' then
    return sprintf('(%s)', ret)
  end
  return sprintf('(%s . %s)', ret, printObj(obj))
end

def findVar(sym, env)
  while env['tag'] == 'cons' do
    alist = env['car']
    while alist['tag'] == 'cons' do
      if alist['car']['car'] == sym
        return alist['car']
      end
      alist = alist['cdr']
    end
    env = env['cdr']
  end
  return $kNil
end

$g_env = makeCons($kNil, $kNil)

def addToEnv(sym, val, env)
  env['car'] = makeCons(makeCons(sym, val), env['car'])
end

def eval1(obj, env)
  if obj['tag'] == 'nil' || obj['tag'] == 'num' || obj['tag'] == 'error' then
    return obj
  elsif obj['tag'] == 'sym' then
    bind = findVar(obj, env)
    if bind == $kNil then
      return makeError(sprintf('%s has no value', obj['data']))
    end
    return bind['cdr']
  end

  op = safeCar(obj)
  args = safeCdr(obj)
  if op == makeSym('quote') then
    return safeCar(args)
  elsif op == makeSym('if') then
    if eval1(safeCar(args), env) == $kNil then
      return eval1(safeCar(safeCdr(safeCdr(args))), env)
    end
    return eval1(safeCar(safeCdr(args)), env)
  elsif op == makeSym('lambda') then
    return makeExpr(args, env)
  elsif op == makeSym('defun') then
    expr = makeExpr(safeCdr(args), env)
    sym = safeCar(args)
    addToEnv(sym, expr, $g_env)
    return sym
  elsif op == makeSym('setq') then
    val = eval1(safeCar(safeCdr(args)), env)
    sym = safeCar(args)
    bind = findVar(sym, env)
    if bind == $kNil then
      addToEnv(sym, val, $g_env)
    else
      bind['cdr'] = val
    end
    return val
  end
  return apply(eval1(op, env), evlis(args, env), env)
end

def evlis(lst, env)
  ret = $kNil
  while lst['tag'] == 'cons' do
    elm = eval1(lst['car'], env)
    if elm['tag'] == 'error' then
      return elm
    end
    ret = makeCons(elm, ret)
    lst = lst['cdr']
  end
  return nreverse(ret)
end

def progn(body, env)
  ret = $kNil
  while body['tag'] == 'cons' do
    ret = eval1(body['car'], env)
    body = body['cdr']
    return ret
  end
end

def apply(fn, args, env)
  if fn['tag'] == 'error' then
    return fn
  elsif args['tag'] == 'error' then
    return args
  elsif fn['tag'] == 'subr' then
    return fn['data'].call(args)
  elsif fn['tag'] == 'expr' then
    return progn(fn['body'], makeCons(pairlis(fn['args'], args), fn['env']))
  end
  return makeError('noimpl')
end

$subrCar = lambda { |args|
  safeCar(safeCar(args))
}

$subrCdr = lambda { |args|
  safeCdr(safeCar(args))
}

$subrCons = lambda { |args|
  makeCons(safeCar(args), safeCar(safeCdr(args)))
}

$subrEq = lambda { |args|
  x = safeCar(args)
  y = safeCar(safeCdr(args))
  if x['tag'] == 'num' and y['tag'] == 'num' then
    if x['data'] == y['data'] then
      return makeSym('t')
    end
    return $kNil
  elsif x.equal?(y) then
    return makeSym('t')
  end
  return $kNil
}

$subrAtom = lambda { |args|
  if safeCar(args)['tag'] == 'cons' then
    return $kNil
  end
  return makeSym('t')
}

$subrNumberp = lambda { |args|
  if safeCar(args)['tag'] == 'num' then
    return makeSym('t')
  end
  return $kNil
}

$subrSymbolp = lambda { |args|
  if safeCar(args)['tag'] == 'sym' then
    return makeSym('t')
  end
  return $kNil
}

def subrAddOrMul(fn, init_val)
  return lambda { |args|
    ret = init_val
    while args['tag'] == 'cons' do
      if args['car']['tag'] != 'num' then
        return makeError('wrong type')
      end
      ret = fn.call(ret, args['car']['data'])
      args = args['cdr']
    end
    return makeNum(ret)
  }
end
$subrAdd = subrAddOrMul(lambda{ |x, y| x + y }, 0)
$subrMul = subrAddOrMul(lambda{ |x, y| x * y }, 1)

def subrSubOrDivOrMod(fn)
  return lambda { |args|
    x = safeCar(args)
    y = safeCar(safeCdr(args))
    if x['tag'] != 'num' || y['tag'] != 'num' then
      return makeError('wrong type')
    end
    return makeNum(fn.call(x['data'], y['data']))
  }
end
$subrSub = subrSubOrDivOrMod(lambda{ |x, y| x - y })
$subrDiv = subrSubOrDivOrMod(lambda{ |x, y| x / y })
$subrMod = subrSubOrDivOrMod(lambda{ |x, y| x % y })

addToEnv(makeSym('car'), makeSubr($subrCar), $g_env)
addToEnv(makeSym('cdr'), makeSubr($subrCdr), $g_env)
addToEnv(makeSym('cons'), makeSubr($subrCons), $g_env)
addToEnv(makeSym('eq'), makeSubr($subrEq), $g_env)
addToEnv(makeSym('atom'), makeSubr($subrAtom), $g_env)
addToEnv(makeSym('numberp'), makeSubr($subrNumberp), $g_env)
addToEnv(makeSym('symbolp'), makeSubr($subrSymbolp), $g_env)
addToEnv(makeSym('+'), makeSubr($subrAdd), $g_env)
addToEnv(makeSym('*'), makeSubr($subrMul), $g_env)
addToEnv(makeSym('-'), makeSubr($subrSub), $g_env)
addToEnv(makeSym('/'), makeSubr($subrDiv), $g_env)
addToEnv(makeSym('mod'), makeSubr($subrMod), $g_env)
addToEnv(makeSym('t'), makeSym('t'), $g_env)


while true
  print('> ')
  str = STDIN.gets
  break if not str
  exp, _ = read(str)
  puts printObj(eval1(exp, $g_env))
end
