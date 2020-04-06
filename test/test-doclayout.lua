--
-- Tests for the doclayout module
--
local doclayout = require 'doclayout'
local tasty = require 'tasty'

local group = tasty.test_group
local test = tasty.test_case
local assert = tasty.assert

-- Check existence static fields
return {
  group 'static fields' {
    test('empty_doc', function ()
      assert.are_equal(type(doclayout.empty_doc), 'userdata')
    end),
  },
}
