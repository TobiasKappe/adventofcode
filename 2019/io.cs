using System;
using System.Numerics;
using System.Text;

namespace @__default {

public partial class IO
{
    public static void Read(out BigInteger read)
    {
        read = BigInteger.Parse(Console.In.ReadLine());
    }

    public static void ReadUntil(char split, out BigInteger read)
    {
        StringBuilder buf = new StringBuilder();
        while (Console.In.Peek() > -1) {
            var next = (char) Console.In.Read();
            if ((char) next == split) {
                break;
            }
            buf.Append(next);
        }
        read = BigInteger.Parse(buf.ToString());
    }

    public static void Available(out bool available)
    {
        available = Console.In.Peek() > -1;
    }
}

}
