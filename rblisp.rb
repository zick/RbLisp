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
    return makeError('noimpl')
  elsif str[0] == $kQuote then
    return makeError('noimpl')
  else
    return readAtom(str)
  end
end

while str = STDIN.gets
  puts read(str)
end
