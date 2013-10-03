/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package swingame.platform;

import java.awt.Point;

import swingame.InputAdapter;

/**
 *
 * @author acain
 */
public class NativeInput implements InputAdapter
{
    private static native void n_mousePosition(Point p);    
    private static native boolean n_mouseClicked(int button);
    private static native boolean n_mouseDown(int button);
    private static native boolean n_keyTyped(int key);    
    private static native boolean n_keyDown(int key);
    private static native boolean n_windowCloseRequested();
    private static native void n_processEvents();
    
    public Point mousePosition() 
    {
        Point p = new Point();
        n_mousePosition(p);        
        return p;
    }
    public boolean mouseClicked(int button) { return n_mouseClicked(button); }
    public boolean mouseDown(int button) { return n_mouseDown(button); }
    public boolean keyTyped(int key) { return n_keyTyped(key); }    
    public boolean keyDown(int key) { return n_keyDown(key); };
    public boolean windowCloseRequested() { return n_windowCloseRequested(); }
    public void processEvents() { n_processEvents(); }
    
}
