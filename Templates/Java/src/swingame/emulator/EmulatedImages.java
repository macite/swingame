package swingame.emulator;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.Toolkit;

import java.util.Map;
import java.util.HashMap;

import java.io.File;

import swingame.ImagesAdapter;

/**
 * <<Class summary>>
 *
 * @author   &lt;&gt;
 * @version $Rev$
 */
public final class EmulatedImages implements ImagesAdapter
{
	private Map<String, Image> _images = new HashMap<String, Image>();

	public void loadBitmapNamed(String name, String filename)
	{
		String fullName = new java.io.File("").getAbsolutePath() + File.separator + "Resources" + File.separator + "images" + File.separator + filename;

		Toolkit toolkit = Toolkit.getDefaultToolkit();
		Image img = toolkit.getImage(fullName);
		_images.put(name, img);
	}

    public void drawBitmap(String name, float x, float y)
    {
    	Image img = _images.get(name);
    	if (img == null) return;

    	Graphics g = EmulatedGraphics.getWindow().getScreenGraphics();
    	g.drawImage(img, (int)x, (int)y, null);
    }
}