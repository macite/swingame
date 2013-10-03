/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package swingame;

import java.awt.Color;

import swingame.emulator.EmulatedImages;

/**
 *
 * @author acain
 */
public class Images 
{
    private static ImagesAdapter _ia = new EmulatedImages();

    public static void loadBitmapNamed(String name, String filename)
    {
    	_ia.loadBitmapNamed(name, filename);
    }

    public static void drawBitmap(String name, float x, float y)
    {
    	_ia.drawBitmap(name, x, y);
    }

}