package swingame;

import java.util.Random;

/**
 * <<Class summary>>
 *
 * @author Andrew Cain &lt;&gt;
 * @version $Rev$
 */
public final class Utils 
{
    private static Random _r = new Random();
    
    public static void useNative()
    {
        Graphics.useNative();
        Input.useNative();
    }
    
    
    public static float rnd()
    {
        return _r.nextFloat();
    }
    
    public static int rnd(int upTo)
    {
        return _r.nextInt(upTo);
    }
    
    public static void main(String args[])
    {
        Graphics.openGraphicsWindow("Hello Java World", 800, 600);
        
        do
        {
            Input.processEvents();
            Graphics.refreshScreen();
        } while ( false == Input.windowCloseRequested());
    }
}
