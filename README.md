bgfx-idl is a Rust parser for bgfx idl files https://github.com/bkaradzic/bgfx/blob/master/scripts/bgfx.idl

The purpose is to use this data to generate bindings for bgfx, but the format can be used for other projects as well.

The bgfx idl format is Lua based so this code uses https://github.com/Kampfkarren/full-moon which is a Lua parser written in Rust, but what bgfx-idl offers is a bit easier access to the data instead of navigating the Lua AST tree. It also does some of the calculations requried for flags and such.

Some example of bgfx.idl syntax

```Lua
--- Depth test state. When `BGFX_STATE_DEPTH_` is not specified depth test will be disabled.
flag.StateDepthTest { bits = 64, shift = 4, range = 4, base = 1 , desc = "Depth test state" }
	.Less     --- Enable depth test, less.
	.Lequal   --- Enable depth test, less or equal.
	.Equal    --- Enable depth test, equal.
	.Gequal   --- Enable depth test, greater or equal.
	.Greater  --- Enable depth test, greater.
	.Notequal --- Enable depth test, not equal.
	.Never    --- Enable depth test, never.
	.Always   --- Enable depth test, always.
	()

--- Init attachment.
func.Attachment.init
	"void"
	.handle "TextureHandle" --- Render target texture handle.
	.access "Access::Enum"  --- Access. See `Access::Enum`.
	 { default = "Access::Write" }
	.layer "uint16_t"       --- Cubemap side or depth layer/slice to use.
	 { default = 0 }
	.numLayers "uint16_t"   --- Number of texture layer/slice(s) in array to use.
	 { default = 1 }
	.mip "uint16_t"         --- Mip level.
	 { default = 0 }
	.resolve "uint8_t"      --- Resolve flags. See: `BGFX_RESOLVE_*`
	 { default = "BGFX_RESOLVE_AUTO_GEN_MIPS" }
```
