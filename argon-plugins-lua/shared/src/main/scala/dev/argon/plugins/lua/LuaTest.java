package dev.argon.plugins.lua;

import com.sun.jna.Callback;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;

public class LuaTest {

	public interface LuaLib extends Library {
		LuaLib INSTANCE = Native.load("lua5.4", LuaLib.class);

		int LUA_OK = 0;
		int LUA_MULTIRET = -1;

		Pointer luaL_newstate();

		void luaL_openlibs(Pointer luaState);

		int luaL_loadstring(Pointer luaState, String luaCode);

		int lua_pcallk(Pointer luaState, int nargs, int nresults, int errfunc, Pointer ctx, LuaKFunction k);
		
		String lua_tolstring(Pointer luaState, int idx, Pointer len);

		void lua_close(Pointer luaState);
	}

	public interface LuaKFunction extends Callback {
		int call(Pointer luaState, int status, Pointer ctx);
	}

	public static void main(String[] args) {
		Pointer l = LuaLib.INSTANCE.luaL_newstate();
		try {
			LuaLib.INSTANCE.luaL_openlibs(l);

			String luaCode = "print('Hello, world from Lua!')";

			if (
				LuaLib.INSTANCE.luaL_loadstring(l, luaCode) != LuaLib.LUA_OK ||
					LuaLib.INSTANCE.lua_pcallk(l, 0, LuaLib.LUA_MULTIRET, 0, Pointer.NULL, null) != LuaLib.LUA_OK
			) {
				System.err.format("Error executing Lua code: %s%n", LuaLib.INSTANCE.lua_tolstring(l, -1, null));
			}
		}
		finally {
			LuaLib.INSTANCE.lua_close(l);
		}
	}

}
