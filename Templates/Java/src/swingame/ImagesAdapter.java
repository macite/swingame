package swingame;

import java.awt.Color;

/**
 * <<Class summary>>
 *
 * @author Andrew Cain &lt;&gt;
 * @version $Rev$
 */
public interface ImagesAdapter 
{
    public void loadBitmapNamed(String name, String filename);
    public void drawBitmap(String name, float x, float y);
}
