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
end

addToEnv(makeSym('t'), makeSym('t'), $g_env)

while str = STDIN.gets
  exp, _ = read(str)
  puts printObj(eval1(exp, $g_env))
end
