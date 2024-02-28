module HRayLib3d.GameEngine.Realm.Physics where 

import HRayLib3d.GameEngine.Realm.World (World)

type GravityConstant = Float
data Physics = Physics {world :: World, const :: GravityConstant} deriving (Eq, Show)

instance Eq   World

-- TODO: Refactor code into rigidbody functionality
-- #include "core/physics/Physics.h"

-- #include "core/components/internal/WorldTransform.h"
-- #include "core/components/synchronized/RigidBodyComponent.h"
-- #include "core/world/World.h"
-- #include "log/log.h"

-- update :: Double -> IO () 
-- update dt = do
--   dynamics_world->stepSimulation(dt);
--   auto& registry = world->registry;
--     log_zone_named("Refresh dead RigidBodys");

--     auto rigid_body_view = registry ^. rigidBodyComponent

--     for (auto e : rigid_body_view) 
--       auto& rigid_body = rigid_body_view.get(e);

--       if (rigid_body._rigid_body == nullptr) 
--         float body_mass = rigid_body._data.mass();

--         -- TODO(marceline-cramer) RigidBodyComponent sets its own transform
--         btTransform body_transform;
--         body_transform.setIdentity();
--         body_transform.setOrigin(btVector3(0.0, 10.0, 0.0));
--         btMotionState* motion_state = new btDefaultMotionState(body_transform);

--         btVector3 body_inertia;
--         default_shape->calculateLocalInertia(body_mass, body_inertia);

--         btRigidBody::btRigidBodyConstructionInfo ci(
--             body_mass, motion_state, default_shape, body_inertia);
--         btRigidBody* new_body = new btRigidBody(ci);

--         rigid_body._rigid_body = new_body;
--         rigid_body._motion_state = motion_state;

--         dynamics_world->addRigidBody(new_body);

--     log_zone_named("Update Transform from RigidBody");

--     auto rigid_body_view = registry.view<RigidBodyComponent>();

--     for (auto e : rigid_body_view) {
--       auto& rigid_body = rigid_body_view.get(e);
--       auto new_transform = rigid_body.makeWorldTransform();
--       registry.emplace_or_replace<WorldTransform>(e, new_transform);
--     }

-- onRigidBodyComponentDestroy :: EntityRegistry -> EntityId -> RigidBody
-- onRigidBodyComponentDestroy registry id = do
--   let rigid_body = getWithRegistry registry id "RigidBodyComponent"
--       self       = getCtx "Physics" registry

-- --   if _rigid_body rigid_body != nullptr
-- --     if (dynamics_world self  != nullptr) {
-- --       self->dynamics_world->removeRigidBody(rigid_body._rigid_body);
-- --     }

--     delete rigid_body._rigid_body;
--     rigid_body._rigid_body = nullptr;
  
--     delete rigid_body._motion_state;
--     rigid_body._motion_state = nullptr;

