include "io.dfy"

function method fuel(mass: int): int
{
    ((mass as real)/3.0).Floor - 2
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
        total := total + fuel(mass);
        available := io.Available();
    }

    print total, "\n";
}
