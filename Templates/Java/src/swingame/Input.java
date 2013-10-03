/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package swingame;

import java.awt.Point;

import swingame.emulator.EmulatedInput;
import swingame.platform.NativeInput;

/**
 *
 * @author acain
 */
public class Input 
{
    private static InputAdapter _ia = new EmulatedInput();
    
    protected static void useNative()
    {
        _ia = new NativeInput();
    }
    
    public static Point mousePosition() 
    {
        return _ia.mousePosition();
    }
    
    public static boolean mouseClicked(int button) { return _ia.mouseClicked(button); }
    public static boolean mouseDown(int button) { return _ia.mouseDown(button); }
    public static boolean keyTyped(int key) { return _ia.keyTyped(key); }
    public static boolean keyDown(int key) { return _ia.keyDown(key); }
    public static boolean windowCloseRequested() { return _ia.windowCloseRequested(); }
    public static void processEvents() { _ia.processEvents(); }
    
    
}
