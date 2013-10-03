package swingame;

import java.awt.Point;

/**
 * <<Class summary>>
 *
 * @author Andrew Cain &lt;&gt;
 * @version $Rev$
 */
public interface InputAdapter 
{
    public Point mousePosition();
    public boolean mouseClicked(int button);
    public boolean mouseDown(int button);
    public boolean keyTyped(int key);    
    public boolean keyDown(int key);
    
    public boolean windowCloseRequested();
    public void processEvents();
    
    
}
