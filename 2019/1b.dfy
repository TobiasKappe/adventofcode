include "io.dfy"

function method fuel(mass: int): int
{
    ((mass as real)/3.0).Floor - 2
}

method FixFuel(mass: int) returns (total: int)
{
    var added := mass;
    total := 0;
    while fuel(added) > 0 
    {
        added := fuel(added);
        total := total + added;
    }
}

method {:main} Main()
    decreases *
{
    var io: IO;
    var available := true;
    var total := 0;

    while available 
        decreases *
    {
        var mass := io.Read();
        var needed := FixFuel(mass);
        total := total + needed;
        available := io.Available();
    }

    print total, "\n";
}
