using System;
using System.Collections.Generic;
using System.Text;

namespace SwinGame
{
    internal static class WrapperUtils
    {
/*        internal static Matrix2D MatrixFromArray(float[,] data)
        {
            Matrix2D result;
            result._data = data;
            return result;
        }
        
        internal static Triangle TriangleFromArray(Point2D[] data)
        {
            Triangle result;
            result._data = data;
            return result;
        }
*/        
/*        internal static Triangle[] TriangleArrayFrom(Point2D[] data)
        {
            Triangle[] result = new Triangle[data.Length / 3];
            
            for(int i = 0; i < data.Length / 3; i++)
            {
                result[i]._data = new Point2D[3];
                
                for(int j = 0; j < 3; j++)
                {
                    result[i]._data[j] = data[i * 3 + j];
                }
            }
            
            return result;
        }
*/        
        internal static StringBuilder[] ResultStringArray(int size)
        {
            StringBuilder[] result = new StringBuilder[size];
            for (int i = 0; i < size; i++) result[i] = new StringBuilder(2048);
            
            return result;
        }
        
        internal static string[] StringArrayFrom(StringBuilder[] data)
        {
            string[] result = new string[data.Length];
            
            for(int i = 0; i < data.Length; i++)
            {
                result[i] = data[i].ToString();
            }
            
            return result;
        }
        
/*        internal static Point2D[] TriangleArrToPoint2DArr(Triangle[] data)
        {
            Point2D[] result = new Point2D[data.Length * 3];
            
            for(int i = 0; i < data.Length; i++)
            {
                result[i * 3] = data[i][0];
                result[i * 3 + 1] = data[i][1];
                result[i * 3 + 2] = data[i][2];
            }
            
            return result;
        }
*/        
        internal static Bitmap[] BitmapArrayFrom(IntPtr[] data)
        {
            Bitmap[] result = new Bitmap[data.Length];
            
            for(int i = 0; i < data.Length; i++)
            {
                result[i] = Bitmap.Create(data[i]);
            }
            return result;
        }
        
        internal static IntPtr[] BitmapArrToIntPtrArr(IntPtr[] bmps) { return bmps; }
        
        internal static IntPtr[] BitmapArrToIntPtrArr(Bitmap[] data)
        {
            IntPtr[] result = new IntPtr[data.Length];
            
            for(int i = 0; i < data.Length; i++)
            {
                result[i] = data[i];
            }
            return result;
        }
    }
}