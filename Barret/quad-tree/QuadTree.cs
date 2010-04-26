using System;
using System.Collections.Generic;
using Microsoft.Xna.Framework;

namespace Application
{
    public class QuadTree
    {
        public readonly Vector2 Position;   // Center position
        public readonly Vector2 Size;       // Bounds of the space
        public readonly int Level;          // The level of this space
        public readonly int MaxLevel;       // Max number of levels in the quad tree hierarchy

        private QuadTree uLeftQuad;    // Upper left quad space
        private QuadTree lLeftQuad;    // Lower left quad space
        private QuadTree uRightQuad;   // Upper right quad space
        private QuadTree lRightQuad;   // Lower right quad space

        public readonly List<IRenderable> Objects;

        /// <summary>
        /// A quad tree for spacial organization. Breaks down a 2D area into a grid for more intelligent
        /// searching.
        /// </summary>
        /// <param name="_x"> Center X position</param>
        /// <param name="_y"> Center Y position</param>
        /// <param name="_x_size"></param>
        /// <param name="_z_size"></param>
        /// <param name="_level"></param>
        /// <param name="_maxLevel"></param>
        public QuadTree( Vector2 _position, Vector2 _size, int _level, int _maxLevel )
        {
            Position = _position;
            Size = _size;
            Level = _level;
            MaxLevel = _maxLevel;
            Objects = new List<IRenderable>();

            // If we aren't on the max level, construct our child quad tree spaces
            if ( Level < MaxLevel )
            {
                Vector2 uLeftPosition = Position + new Vector2( -Size.X / 2f, Size.Y / 2f );
                uLeftQuad = new QuadTree( uLeftPosition, Size / 2f, Level + 1, MaxLevel );

                Vector2 lLeftPosition = Position + new Vector2( -Size.X / 2f, -Size.Y / 2f );
                lLeftQuad = new QuadTree( lLeftPosition, Size / 2f, Level + 1, MaxLevel );

                Vector2 uRightPosition = Position + new Vector2( Size.X / 2f, Size.Y / 2f );
                uRightQuad = new QuadTree( uRightPosition, Size / 2f, Level + 1, MaxLevel );

                Vector2 lRightPosition = Position + new Vector2( Size.X / 2f, -Size.Y / 2f );
                lRightQuad = new QuadTree( lRightPosition, Size / 2f, Level + 1, MaxLevel );
            }
            else
            {
                uLeftQuad = null;
                lLeftQuad = null;
                uRightQuad = null;
                lRightQuad = null;
            }
        }

        /// <summary>
        /// Place an object inside the correct quad
        /// </summary>
        /// <param name="o"></param>
        public void placeObject( IRenderable o )
        {
            // If this is the max level, add the object to this space
            if ( Level == MaxLevel )
            {
                Objects.Add( o );
            }
            // Otherwise, search for the child quad space to place the object in
            else
            {
                // Is it on left side?
                if ( o.Position.X <= Position.X )
                {
                    // Is it on top side?
                    if ( o.Position.Y <= Position.Y )
                    {
                        uLeftQuad.placeObject( o );
                    }
                    // Must be on bottom side?
                    else
                    {
                        lLeftQuad.placeObject( o );
                    }
                }
                // Must be on right side
                else
                {
                    // Is it on top side?
                    if ( o.Position.Y <= Position.Y )
                    {
                        uRightQuad.placeObject( o );
                    }
                    // Must be on bottom side?
                    else
                    {
                        lRightQuad.placeObject( o );
                    }
                }
            }
        }

        /// <summary>
        /// Clears all objects out of the tree
        /// </summary>
        public void clearObjects()
        {
            Objects.Clear();   // Clear the objects in this quad space

            // If we aren't at the max level, clear out the child quads
            if( Level < MaxLevel )
            {
                uLeftQuad.clearObjects();
                lLeftQuad.clearObjects();
                uRightQuad.clearObjects();
                lRightQuad.clearObjects();
            }
        }

        /// <summary>
        /// Pass in a position, it finds the quad space that position is in and returns the objects.
        /// </summary>
        /// <param name="position"> Position to check against</param>
        public List<IRenderable> getObjectsInQuad( Vector2 pos )
        {
            // If this is the max level, return these objects
            if ( Level == MaxLevel )
            {
                return Objects;
            }
            // Otherwise, search for the child quad space to place the object in
            else
            {
                // Is it on left side?
                if ( pos.X <= Position.X )
                {
                    // Is it on top side?
                    if ( pos.Y <= Position.Y )
                    {
                        return uLeftQuad.getObjectsInQuad( pos );
                    }
                    // Must be on bottom side?
                    else
                    {
                        return lLeftQuad.getObjectsInQuad( pos );
                    }
                }
                // Must be on right side
                else
                {
                    // Is it on top side?
                    if ( pos.Y <= Position.Y )
                    {
                        return uRightQuad.getObjectsInQuad( pos );
                    }
                    // Must be on bottom side?
                    else
                    {
                        return lRightQuad.getObjectsInQuad( pos );
                    }
                }
            }
        }
    }

    public abstract class IRenderable
    {
        public Vector3 Position;
    }
}
