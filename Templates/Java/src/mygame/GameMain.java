package swingame.samples;

import java.awt.Color;
import java.awt.Point;
import swingame.Graphics;
import swingame.Input;
import swingame.Images;

import swingame.MouseButton;
import swingame.KeyCode;

/**
 * <<Class summary>>
 *
 * @author   &lt;&gt;
 * @version $Rev$
 */
public final class GameMain
{
    public static void main(String[] args)
    {
        //Use the native library using: 
        //Core.useNative();
        
        Graphics.openGraphicsWindow("Hello World", 800, 600);
        
        Images.loadBitmapNamed("swin", "Swinburne.jpg");

        float x = 0.0f;
        Color c = Color.RED;
        Color c1 = Color.BLUE;
        
        while (false == Input.windowCloseRequested())
        {
            Input.processEvents();
            
            if (Input.mouseClicked(MouseButton.LEFT_BUTTON)) 
            {
                c = c == Color.RED ? Color.WHITE : Color.RED;
            }
            
            if (Input.keyTyped(KeyCode.VK_SPACE))
            {
                c1 = c1 == Color.BLUE ? Color.WHITE : Color.BLUE;
            }
            
            Graphics.clearScreen(Color.BLACK);
            Graphics.fillRectangle(c, x, 10.0f, 10, 20);
            Graphics.fillEllipse(c1, x, 40.0f, 20, 10);

            Images.drawBitmap("swin", 50, 50);    
            
            if( false == Input.keyDown(KeyCode.VK_L) )
                Graphics.drawLine(Color.WHITE, x, 70.0f, x + 10, 75.0f);
            
            x += 1;
            
            Point p = Input.mousePosition();
            Graphics.drawPixel(Color.GREEN, (float)p.getX(), (float)p.getY());
            
            Graphics.refreshScreen();
        }
    }
}
