using System;
using System.Numerics;

namespace @__default {

public partial class IO
{
    public static void Read(out BigInteger read)
    {
        read = BigInteger.Parse(Console.In.ReadLine());
    }

    public static void Available(out bool available)
    {
        available = Console.In.Peek() > -1;
    }
}

}
