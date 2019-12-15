include "io.dfy"

type CodeMap = map<int, int>

method CodeGet(codes: CodeMap, position: int)
    returns (newcodes: CodeMap, value: int)
    ensures position in newcodes.Keys
    ensures newcodes[position] == value
{
    if position in codes.Keys
    {
        newcodes := codes;
    }
    else
    {
        newcodes := codes[position := 0];
    }

    value := newcodes[position];
}

method SimulateAdd(codes: CodeMap, position: int)
    returns (newcodes: CodeMap, newposition: int)
{
    var source1, source2, target;
    newcodes := codes;
    newcodes, source1 := CodeGet(newcodes, position+1);
    newcodes, source2 := CodeGet(newcodes, position+2);
    newcodes, target := CodeGet(newcodes, position+3);
    
    var val1, val2;
    newcodes, val1 := CodeGet(newcodes, source1);
    newcodes, val2 := CodeGet(newcodes, source2);

    newcodes := newcodes[target := val1 + val2];
    newposition := position + 4;
}

method SimulateMul(codes: CodeMap, position: int)
    returns (newcodes: CodeMap, newposition: int)
{
    var source1, source2, target;
    newcodes := codes;
    newcodes, source1 := CodeGet(newcodes, position+1);
    newcodes, source2 := CodeGet(newcodes, position+2);
    newcodes, target := CodeGet(newcodes, position+3);
    
    var val1, val2;
    newcodes, val1 := CodeGet(newcodes, source1);
    newcodes, val2 := CodeGet(newcodes, source2);

    newcodes := newcodes[target := val1 * val2];
    newposition := position + 4;
}

method SimulateStep(codes: CodeMap, position: int)
    returns (newcodes: CodeMap, newposition: int, halted: bool)
{
    var code;

    halted := false;
    newcodes, code := CodeGet(codes, position);

    if code == 1
    {
        newcodes, newposition := SimulateAdd(newcodes, position);
    }
    else if code == 2
    {
        newcodes, newposition := SimulateMul(newcodes, position);
    }
    else if code == 99
    {
        halted := true;
    }
}

method Simulate(codes: CodeMap, noun: int, verb: int)
    returns (result: int)
    decreases *
{
    var newcodes := codes;
    newcodes := newcodes[1 := noun];
    newcodes := newcodes[2 := verb];

    var halted := false;
    var position := 0;
    while !halted
        decreases *
    {
        newcodes, position, halted := SimulateStep(newcodes, position);
    }

    newcodes, result := CodeGet(newcodes, 0);
}

method Read()
    returns (codes: CodeMap)
    decreases *
{
    var io: IO;
    var available := true;
    var i := 0;

    codes := map[];
    while available 
        decreases *
    {
        var code := io.ReadUntil(',');
        codes := codes[i := code];
        i := i + 1;
        available := io.Available();
    }
}

method {:main} Main()
    decreases *
{
    var codes := Read();

    var result := Simulate(codes, 12, 2);
    print result, "\n";
}
