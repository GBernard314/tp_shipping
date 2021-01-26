module Shipping (module ShippingShared, module Shipping) where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import ShippingShared

-- the following imports might be useful at some point for advanced users
-- import           Control.Monad.Writer.Strict
-- import           Data.Maybe                  (mapMaybe)

-- | This is YOUR data type that defines how your state is represented.
-- Choose carefully!
data ShippingState = ShippingState
  {
    _curday :: Day,
    _ord :: [(OrderId, Bool)],
    _sent :: M.Map OrderId Bool
  }
  deriving (Show)

-- | Customize this if you want nicer debug messages
prettyShippingState :: ShippingState -> [String]
prettyShippingState s = [show s]

-- | Increase the stock
-- you should be able to use a function from Data.Map.Strict directly
increaseStock :: Stock -> Stock -> Stock
increaseStock inStock inOrder = M.unionWith (+) inStock inOrder

-- | Remove items from stock, if there are enough
-- if you are new to pure functional programming, try transforming the "items to be removed" in to a list, and using a helper function to iterate through that list
-- if you are experienced, look at mergeA function from Data.Map.Merge.Strict
decreaseStock ::
  -- | the main stock
  Stock ->
  -- | items to be removed
  Stock ->
  -- | the updated stock, if there are enough items
  Maybe Stock
decreaseStock inStock inOrder =
  if any (< 0) curmap
    then Nothing
    else Just (M.filter (/= 0) curmap)
  where
    curmap = M.unionWith (+) (inStock) (M.map negate (inOrder))

-- | return the current day from your `ShippingState` type.
-- you should update the `ShippingState` type to store this information
getDay :: ShippingState -> Day
getDay state = _curday state

-- | create the initial shipping state, with the current day as input.
initialShippingState :: Day -> ShippingState
initialShippingState d = ShippingState d [] M.empty

-- | handle the order messageShippingState
order ::
  OrderId ->
  OrderInformation ->
  -- | current stock
  Stock ->
  -- | current shipping state
  ShippingState ->
  -- | updated stock, shippingstate, and possibly messages
  (Stock, ShippingState, Maybe ShippingInformation)
order ordId ordInf sto (ShippingState day ord sent) = case decreaseStock sto (_orderStock ordInf) of
  Nothing -> (sto, ShippingState day ord sent, Nothing)
  Just val -> (val,ShippingState day (ord++[(ordId, True)]) (M.insert ordId False sent), Just (ShippingInformation ordId (_orderDest ordInf)))


-- | return the list, in order of arrival, of the identifiers of orders waiting for stock to be replenished
getWaitingOrders :: ShippingState -> [OrderId]
getWaitingOrders shipSt = case _ord shipSt of
  [(_, False)] -> undefined
  _ -> undefined

getWaitingTracking :: ShippingState -> [(OrderId, Day)]
getWaitingTracking = error "complete the getWaitingTracking function"

-- | handle new stock being received
-- do not forget the requests must be served *in order*
-- You will probably have to foldl' through the waiting orders
restock ::
  -- | current stock
  Stock ->
  -- | stock received
  Stock ->
  -- | current shipping state
  ShippingState ->
  -- | updated stock, shippingstate, and possibly shipping information
  (Stock, ShippingState, [ShippingInformation])
restock = error "complete the restock function"

-- | This function increases the current day in ShippingState.
-- In order to get the full grade, you should implement this function so that it takes action when things get lost.
-- In particular:
--   * tracking requests can get lost. They must be reissued if no response has been received after 5 days, so that the transporter can pick the order up.
--   * items that have been shipped can also be lost. After 7 days, the following actions must be performed:
--       - for standard delivery, you can assume the item has been delivered
--       - for tracked packages, it means it has been lost, and you must send it again (stocks must be decreased again, a new tracking id must be used, just like a new order)
--       - for dropped packages, if it has been dropped, you can assume it has been delivered. However, if it has not, you must send it again!
-- The implementation of this function can be greatly simplified by using the Writer monad (that logs the orders) and some form of traversal.
advanceDay :: Stock -> ShippingState -> (Stock, ShippingState, [OutEvent])
advanceDay = error "complete the advanceDay function"

getWaitingPickup :: ShippingState -> [(OrderId, Day)]
getWaitingPickup = error "complete the getWaitingPickup function"

getReceived :: ShippingState -> [(OrderId, Day)]
getReceived = error "complete the getReceived function"

getInTransit :: ShippingState -> [(OrderId, Day)]
getInTransit = error "complete the getInTransit function"

handleMessage ::
  -- | initial stock before message handling
  Stock ->
  -- | shipping state after message handling
  ShippingState ->
  InEvent ->
  -- | updated states, with messages
  (Stock, ShippingState, [OutEvent])
handleMessage stock st event =
  case event of
    NewOrder oid oi ->
      let (stock', st', shipping) = order oid oi stock st
       in (stock', st', [maybe (OutofstockMessage oid) Ship shipping])
    Restock nstock ->
      let (stock', st', shippings) = restock stock nstock st
       in (stock', st', map Ship shippings)
    NewDay -> advanceDay stock st
    _ -> error ("complete the branch in handleMessage for: " ++ show event)

handleMessages ::
  Stock ->
  ShippingState ->
  [InEvent] ->
  (Stock, ShippingState, [OutEvent])
handleMessages stock state = foldl' run1 (stock, state, [])
  where
    run1 (curstock, curstate, curout) = fmap (curout ++) . handleMessage curstock curstate
