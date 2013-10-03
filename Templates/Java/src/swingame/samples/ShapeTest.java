package swingame.samples;

import java.awt.Point;

import static org.junit.Assert.*;
import org.junit.Test;
import junit.framework.TestCase;

public final class ShapeTest extends TestCase
{
	static { System.out.println("Loading test class"); }

	public static junit.framework.Test suite() 
	{
System.out.println("Getting tests");
		return new junit.framework.JUnit4TestAdapter(ShapeTest.class);
	}

@Test
public void simpleAdd() {
     
    assertTrue(1 == 1);
}

	@Test 
	public void testAtPoint()
	{
		System.out.println("testing atPoint");
		Shape s = new Rectangle();
		s.setPosition(new Point(10, 10));
		s.setHeight(10);
		s.setWidth(10);

		assertTrue(s.isAt(new Point(15, 15)));	
	}
	
	public static void main(String[] args)
	{
		org.junit.runner.JUnitCore.main("swingame.samples.ShapeTest");
	}
}