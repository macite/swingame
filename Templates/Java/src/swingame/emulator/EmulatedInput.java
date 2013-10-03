/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package swingame.emulator;

import java.awt.Point;

import swingame.InputAdapter;

/**
 *
 * @author acain
 */
public class EmulatedInput implements InputAdapter
{
    public Point mousePosition() 
    {
        return EmulatedGraphics.getWindow().getMousePoint();
    }
    
    public boolean mouseClicked(int button) 
    { 
        return EmulatedGraphics.getWindow().mouseClicked(button); 
    }
    
    public boolean mouseDown(int button) 
    { 
        return EmulatedGraphics.getWindow().mouseDown(button); 
    }
    
    public boolean keyTyped(int key) 
    {
        return EmulatedGraphics.getWindow().keyTyped(key); 
    }
    
    public boolean keyDown(int key) 
    { 
        return EmulatedGraphics.getWindow().keyDown(key); 
    }
    
    public boolean windowCloseRequested()
    {
        return false == EmulatedGraphics.getWindow().isVisible();
    }
    
    public void processEvents()
    {
        EmulatedGraphics.getWindow().processEvents();
    }
    
}
