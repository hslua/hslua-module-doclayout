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
  group 'constructors' {
    test('empty', function ()
      assert.are_equal(type(doclayout.empty), 'userdata')
    end),
    test('blankline', function ()
      assert.are_equal(type(doclayout.blankline), 'userdata')
    end),
    test('cr', function ()
      assert.are_equal(type(doclayout.cr), 'userdata')
    end),
    test('space', function ()
      assert.are_equal(type(doclayout.space), 'userdata')
    end),
  },

  group 'functions' {
    group 'render' {
      test('empty doc', function ()
        assert.are_equal(doclayout.render(doclayout.empty), '')
      end)
    }
  },

  group 'Doc type' {
    test('strings can be used as Doc values', function ()
      assert.are_equal(doclayout.render('hello world'), 'hello world')
    end),

    test('has tostring method', function ()
      local str = 'just a literal string for now'
      assert.are_equal(tostring(str), str)
    end)
  }
}
