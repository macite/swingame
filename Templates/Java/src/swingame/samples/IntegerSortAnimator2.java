package swingame.samples;

import java.awt.Color;
import swingame.Graphics;
import swingame.Input;
import swingame.KeyCode;

/**
 * The IntegerSortAnimator objects store a collection
 * of integer values that can be sorted and randomized. 
 * 
 * @author acain
 */
public class IntegerSortAnimator2 
{
    private final int _max;
    private int[] _data;
    private SortAlgorithm _sortWith;
    
    /**
    * Creates a new IntegerSortAnimator with an specified number of 
    * elements from 0 to a max value.
    *
    * @param count  The number of values to store in this animator
    * @param max    The largest value that can be stored in the data for this animator
    */
    public IntegerSortAnimator2(int count, int max)
    {
        _sortWith = new BubbleSort();
	    _max = max;
        _data = new int[count];
	    randomize();
    }
    
    /**
    * Provides access to the array contained within the IntegerSortAnimator.
    *
    * @return The array containing the data within the object - direct access
    */
    public int[] getData()
    {
        return _data;
    }

    /**
    * Randomize the data within the IntegerSortAnimator. This generates a new random set of data
    * for the animator to sort. The new values will be between 0 and max.
    */
    public void randomize() 
    {
        for(int i = 0; i < _data.length; i++)
        {
            _data[i] = (int)(Math.random() * _max);
        }
    }
    
    /**
    * Draws the data into the current SwinGame graphics window. This 
    * drawing takes the entire screen.
    */
    public void showData()
    {
        Graphics.clearScreen();

        int w = Graphics.screenWidth() / _data.length;
        float scaleHeight = Graphics.screenHeight() / (float)_max;

        for(int i = 0; i < _data.length; i++)
        {         
                float x = w * i;
                float y = (_max - _data[i]) * scaleHeight;
                int h = (int)Math.ceil(_data[i] * scaleHeight);

                Graphics.fillRectangle(Color.RED, x, y, w, h);
        }

        Graphics.refreshScreen(60);
    }
    
    /**
    * Sorts the data within the IntegerSortAnimator, updating the display 
    * at each step of the algorithm.
    */
    public void sort()
    {
        sort(true);
    }
    
    /**
    * Sorts the data within the IntegerSortAnimator. The show parameter
    * is used to indicate if this should be drawn to the screen while it
    * progresses.
    *
    * @param show   Indicates if the animator should redraw the data as it 
    * sorts.
    */
    public void sort(boolean show)    
    {
        _sortWith.sort(this, show);
    }
}
