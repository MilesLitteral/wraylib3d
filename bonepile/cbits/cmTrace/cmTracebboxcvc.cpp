void CM_TraceCapsuleThroughCapsule( traceWork_t *tw, clipHandle_t model ) {
	int i;
	vec3_t mins, maxs;
	vec3_t top, bottom, starttop, startbottom, endtop, endbottom;
	vec3_t offset, symetricSize[2];
	float radius, halfwidth, halfheight, offs, h;

	CM_ModelBounds(model, mins, maxs);
	// test trace bounds vs. capsule bounds
	if ( tw->bounds[0][0] > maxs[0] + RADIUS_EPSILON
		|| tw->bounds[0][1] > maxs[1] + RADIUS_EPSILON
		|| tw->bounds[0][2] > maxs[2] + RADIUS_EPSILON
		|| tw->bounds[1][0] < mins[0] - RADIUS_EPSILON
		|| tw->bounds[1][1] < mins[1] - RADIUS_EPSILON
		|| tw->bounds[1][2] < mins[2] - RADIUS_EPSILON
		) {
		return;
	}
	// top origin and bottom origin of each sphere at start and end of trace
	VectorAdd(tw->start, tw->sphere.offset, starttop);
	VectorSubtract(tw->start, tw->sphere.offset, startbottom);
	VectorAdd(tw->end, tw->sphere.offset, endtop);
	VectorSubtract(tw->end, tw->sphere.offset, endbottom);

	// calculate top and bottom of the capsule spheres to collide with
	for ( i = 0 ; i < 3 ; i++ ) {
		offset[i] = ( mins[i] + maxs[i] ) * 0.5;
		symetricSize[0][i] = mins[i] - offset[i];
		symetricSize[1][i] = maxs[i] - offset[i];
	}
	halfwidth = symetricSize[ 1 ][ 0 ];
	halfheight = symetricSize[ 1 ][ 2 ];
	radius = ( halfwidth > halfheight ) ? halfheight : halfwidth;
	offs = halfheight - radius;
	VectorCopy(offset, top);
	top[2] += offs;
	VectorCopy(offset, bottom);
	bottom[2] -= offs;
	// expand radius of spheres
	radius += tw->sphere.radius;
	// if there is horizontal movement
	if ( tw->start[0] != tw->end[0] || tw->start[1] != tw->end[1] ) {
		// height of the expanded cylinder is the height of both cylinders minus the radius of both spheres
		h = halfheight + tw->sphere.halfheight - radius;
		// if the cylinder has a height
		if ( h > 0 ) {
			// test for collisions between the cylinders
			CM_TraceThroughVerticalCylinder(tw, offset, radius, h, tw->start, tw->end);
		}
	}
	// test for collision between the spheres
	CM_TraceThroughSphere(tw, top, radius, startbottom, endbottom);
	CM_TraceThroughSphere(tw, bottom, radius, starttop, endtop);
}

void CM_TraceThroughLeaf( traceWork_t *tw, cLeaf_t *leaf ) {
	int			k;
	int			brushnum;
	cbrush_t	*b;
	cPatch_t	*patch;

	// trace line against all brushes in the leaf
	for ( k = 0 ; k < leaf->numLeafBrushes ; k++ ) {
		brushnum = cm.leafbrushes[leaf->firstLeafBrush+k];

		b = &cm.brushes[brushnum];
		if ( b->checkcount == cm.checkcount ) {
			continue;	// already checked this brush in another leaf
		}
		b->checkcount = cm.checkcount;

		if ( !(b->contents & tw->contents) ) {
			continue;
		}

		CM_TraceThroughBrush( tw, b );
		if ( !tw->trace.fraction ) {
			return;
		}
	}

	// trace line against all patches in the leaf
#ifdef BSPC
	if (1) {
#else
	if ( !cm_noCurves->integer ) {
#endif
		for ( k = 0 ; k < leaf->numLeafSurfaces ; k++ ) {
			patch = cm.surfaces[ cm.leafsurfaces[ leaf->firstLeafSurface + k ] ];
			if ( !patch ) {
				continue;
			}
			if ( patch->checkcount == cm.checkcount ) {
				continue;	// already checked this patch in another leaf
			}
			patch->checkcount = cm.checkcount;

			if ( !(patch->contents & tw->contents) ) {
				continue;
			}
			
			CM_TraceThroughPatch( tw, patch );
			if ( !tw->trace.fraction ) {
				return;
			}
		}
	}
}


#ifdef ALWAYS_BBOX_VS_BBOX
			if ( model == BOX_MODEL_HANDLE || model == CAPSULE_MODEL_HANDLE) {
				tw.sphere.use = qfalse;
				CM_TraceThroughLeaf( &tw, &cmod->leaf );
			}
			else
#elif defined(ALWAYS_CAPSULE_VS_CAPSULE)
			if ( model == BOX_MODEL_HANDLE || model == CAPSULE_MODEL_HANDLE) {
				CM_TraceCapsuleThroughCapsule( &tw, model );
			}
			else
#endif