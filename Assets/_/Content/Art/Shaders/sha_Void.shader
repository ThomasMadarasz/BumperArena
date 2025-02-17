// Made with Amplify Shader Editor v1.9.1.3
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "FSGShaders/sha_Void"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[ASEBegin][Header(BordersTxtr______________________________)][Header()][NoScaleOffset]_BordersTxtr("BordersTxtr", 2D) = "white" {}
		[Toggle]_RadialUV("RadialUV", Float) = 0
		_RadialBorderTextureCoordination("RadialBorderTextureCoordination", Vector) = (1,1,0,0)
		_RadialBorderTextureSpeed("RadialBorderTextureSpeed", Vector) = (0,0,0,0)
		_RadialBorderOffset("RadialBorderOffset", Vector) = (0.5,0.5,0,0)
		_BordersNoiseSize("BordersNoiseSize", Float) = 0
		_BorderSpeed("BorderSpeed", Vector) = (0,0.025,0,0)
		_BorderTO("BorderTO", Vector) = (1,1,1,0)
		_BordersColor("BordersColor", Color) = (0,0,0,0)
		_BorderMoveUv("BorderMoveUv", Float) = 1
		_NoiseSpeed("NoiseSpeed", Vector) = (0,0.025,0,0)
		_NoiseTO("NoiseTO", Vector) = (1,1,1,0)
		_NoiseUv("NoiseUv", Float) = 1
		[Header(RadialTxtr______________________________)][NoScaleOffset]_RadialTxtr("RadialTxtr", 2D) = "white" {}
		_RadialTextureCoordination("RadialTextureCoordination", Vector) = (1,1,0,0)
		_RadialOffset("RadialOffset", Vector) = (0.5,0.5,0,0)
		_RadialTiling("RadialTiling", Vector) = (0,0,0,0)
		_RadialNoiseSize("RadialNoiseSize", Float) = 0
		_RadialNoiseTO("RadialNoiseTO", Vector) = (1,1,1,0)
		_RadialNoiseMoveUV("RadialNoiseMoveUV", Float) = 1
		_RadialNoiseSpeed("RadialNoiseSpeed", Vector) = (1,0,0,0)
		_RadialTextureSpeed("RadialTextureSpeed", Vector) = (0,0,0,0)
		[HDR]_RadialTxtrColors("RadialTxtrColors", Color) = (0.2792338,0.123398,0.5566038,0)
		[NoScaleOffset]_RadialMask("RadialMask", 2D) = "white" {}
		[Header(CenterTxtr______________________________)][NoScaleOffset]_CenterTxtr("CenterTxtr", 2D) = "white" {}
		_CenterNoiseSize("CenterNoiseSize", Float) = 0
		_CenterNoiseSpeed("CenterNoiseSpeed", Vector) = (1,0,0,0)
		_CenterNoiseTO("CenterNoiseTO", Vector) = (1,1,1,0)
		_CenterNoiseMoveUV("CenterNoiseMoveUV", Float) = 1
		_CenterTxtrSpeed1("CenterTxtrSpeed", Vector) = (1,0,0,0)
		_CenterTxtrTO1("CenterTxtrTO", Vector) = (1,1,1,0)
		_CenterTxtrMoveUV1("CenterTxtrMoveUV", Float) = 1
		[HDR]_CenterTxtrColors("CenterTxtrColors", Color) = (0.2792338,0.123398,0.5566038,0)
		[Header(Nebula______________________________)][Header()][NoScaleOffset]_NebulaTxtr("NebulaTxtr", 2D) = "white" {}
		_NebulaSpeed("NebulaSpeed", Vector) = (0,0.025,0,0)
		_NebulaTO("NebulaTO", Vector) = (1,1,1,0)
		_NebulaMoveUv("NebulaMoveUv", Float) = 1
		_NebulaColor("NebulaColor", Color) = (0.5215687,0.2117647,0.764706,1)
		[Header(AlphaNoise______________________________)][NoScaleOffset]_AlphaTxtr("AlphaTxtr", 2D) = "white" {}
		_AlphaNSpeed("AlphaNSpeed", Vector) = (0,0.025,0,0)
		_AlphaTextureSpeed("AlphaTextureSpeed", Vector) = (0,0.025,0,0)
		_AlphaNTO("AlphaNTO", Vector) = (1,1,1,0)
		_AlphaTextureTO("AlphaTextureTO", Vector) = (1,1,1,0)
		_AlphaNMoveUv("AlphaNMoveUv", Float) = 1
		_AlphaTextureMoveUv("AlphaTextureMoveUv", Float) = 1
		[ASEEnd]_AlphaNoiseSize("AlphaNoiseSize", Float) = 0
		[HideInInspector] _texcoord2( "", 2D ) = "white" {}


		//_TessPhongStrength( "Tess Phong Strength", Range( 0, 1 ) ) = 0.5
		//_TessValue( "Tess Max Tessellation", Range( 1, 32 ) ) = 16
		//_TessMin( "Tess Min Distance", Float ) = 10
		//_TessMax( "Tess Max Distance", Float ) = 25
		//_TessEdgeLength ( "Tess Edge length", Range( 2, 50 ) ) = 16
		//_TessMaxDisp( "Tess Max Displacement", Float ) = 25

		[HideInInspector] _QueueOffset("_QueueOffset", Float) = 0
        [HideInInspector] _QueueControl("_QueueControl", Float) = -1

        [HideInInspector][NoScaleOffset] unity_Lightmaps("unity_Lightmaps", 2DArray) = "" {}
        [HideInInspector][NoScaleOffset] unity_LightmapsInd("unity_LightmapsInd", 2DArray) = "" {}
        [HideInInspector][NoScaleOffset] unity_ShadowMasks("unity_ShadowMasks", 2DArray) = "" {}
	}

	SubShader
	{
		LOD 0

		

		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Transparent" "Queue"="Transparent" "UniversalMaterialType"="Unlit" }

		Cull Back
		AlphaToMask Off

		

		HLSLINCLUDE
		#pragma target 3.5
		#pragma prefer_hlslcc gles
		// ensure rendering platforms toggle list is visible

		#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Common.hlsl"
		#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Filtering.hlsl"

		#ifndef ASE_TESS_FUNCS
		#define ASE_TESS_FUNCS
		float4 FixedTess( float tessValue )
		{
			return tessValue;
		}

		float CalcDistanceTessFactor (float4 vertex, float minDist, float maxDist, float tess, float4x4 o2w, float3 cameraPos )
		{
			float3 wpos = mul(o2w,vertex).xyz;
			float dist = distance (wpos, cameraPos);
			float f = clamp(1.0 - (dist - minDist) / (maxDist - minDist), 0.01, 1.0) * tess;
			return f;
		}

		float4 CalcTriEdgeTessFactors (float3 triVertexFactors)
		{
			float4 tess;
			tess.x = 0.5 * (triVertexFactors.y + triVertexFactors.z);
			tess.y = 0.5 * (triVertexFactors.x + triVertexFactors.z);
			tess.z = 0.5 * (triVertexFactors.x + triVertexFactors.y);
			tess.w = (triVertexFactors.x + triVertexFactors.y + triVertexFactors.z) / 3.0f;
			return tess;
		}

		float CalcEdgeTessFactor (float3 wpos0, float3 wpos1, float edgeLen, float3 cameraPos, float4 scParams )
		{
			float dist = distance (0.5 * (wpos0+wpos1), cameraPos);
			float len = distance(wpos0, wpos1);
			float f = max(len * scParams.y / (edgeLen * dist), 1.0);
			return f;
		}

		float DistanceFromPlane (float3 pos, float4 plane)
		{
			float d = dot (float4(pos,1.0f), plane);
			return d;
		}

		bool WorldViewFrustumCull (float3 wpos0, float3 wpos1, float3 wpos2, float cullEps, float4 planes[6] )
		{
			float4 planeTest;
			planeTest.x = (( DistanceFromPlane(wpos0, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[0]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.y = (( DistanceFromPlane(wpos0, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[1]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.z = (( DistanceFromPlane(wpos0, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[2]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.w = (( DistanceFromPlane(wpos0, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[3]) > -cullEps) ? 1.0f : 0.0f );
			return !all (planeTest);
		}

		float4 DistanceBasedTess( float4 v0, float4 v1, float4 v2, float tess, float minDist, float maxDist, float4x4 o2w, float3 cameraPos )
		{
			float3 f;
			f.x = CalcDistanceTessFactor (v0,minDist,maxDist,tess,o2w,cameraPos);
			f.y = CalcDistanceTessFactor (v1,minDist,maxDist,tess,o2w,cameraPos);
			f.z = CalcDistanceTessFactor (v2,minDist,maxDist,tess,o2w,cameraPos);

			return CalcTriEdgeTessFactors (f);
		}

		float4 EdgeLengthBasedTess( float4 v0, float4 v1, float4 v2, float edgeLength, float4x4 o2w, float3 cameraPos, float4 scParams )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;
			tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
			tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
			tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
			tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			return tess;
		}

		float4 EdgeLengthBasedTessCull( float4 v0, float4 v1, float4 v2, float edgeLength, float maxDisplacement, float4x4 o2w, float3 cameraPos, float4 scParams, float4 planes[6] )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;

			if (WorldViewFrustumCull(pos0, pos1, pos2, maxDisplacement, planes))
			{
				tess = 0.0f;
			}
			else
			{
				tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
				tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
				tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
				tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			}
			return tess;
		}
		#endif //ASE_TESS_FUNCS
		ENDHLSL

		
		Pass
		{
			
			Name "Forward"
			Tags { "LightMode"="UniversalForwardOnly" }

			Blend SrcAlpha OneMinusSrcAlpha, One OneMinusSrcAlpha
			ZWrite Off
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA

			

			HLSLPROGRAM

			#pragma multi_compile_instancing
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_SRP_VERSION 120108


			#pragma multi_compile _ _DBUFFER_MRT1 _DBUFFER_MRT2 _DBUFFER_MRT3

			#pragma multi_compile _ LIGHTMAP_ON
			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			#pragma shader_feature _ _SAMPLE_GI
			#pragma multi_compile _ DEBUG_DISPLAY

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS SHADERPASS_UNLIT

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DBuffer.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Debug/Debugging3D.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Input.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/SurfaceData.hlsl"

			

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					float4 shadowCoord : TEXCOORD1;
				#endif
				#ifdef ASE_FOG
					float fogFactor : TEXCOORD2;
				#endif
				float4 ase_texcoord3 : TEXCOORD3;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BordersColor;
			float4 _CenterTxtrTO1;
			float4 _NebulaColor;
			float4 _NebulaTO;
			float4 _RadialTxtrColors;
			float4 _CenterTxtrColors;
			float4 _RadialNoiseTO;
			float4 _AlphaTextureTO;
			float4 _NoiseTO;
			float4 _CenterNoiseTO;
			float4 _AlphaNTO;
			float4 _BorderTO;
			float2 _RadialBorderTextureCoordination;
			float2 _CenterTxtrSpeed1;
			float2 _BorderSpeed;
			float2 _NebulaSpeed;
			float2 _AlphaNSpeed;
			float2 _NoiseSpeed;
			float2 _RadialTiling;
			float2 _RadialBorderTextureSpeed;
			float2 _RadialNoiseSpeed;
			float2 _RadialOffset;
			float2 _CenterNoiseSpeed;
			float2 _RadialTextureSpeed;
			float2 _RadialTextureCoordination;
			float2 _RadialBorderOffset;
			float2 _AlphaTextureSpeed;
			float _CenterNoiseSize;
			float _CenterNoiseMoveUV;
			float _AlphaTextureMoveUv;
			float _RadialNoiseSize;
			float _NebulaMoveUv;
			float _AlphaNMoveUv;
			float _RadialNoiseMoveUV;
			float _BordersNoiseSize;
			float _NoiseUv;
			float _BorderMoveUv;
			float _RadialUV;
			float _CenterTxtrMoveUV1;
			float _AlphaNoiseSize;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _BordersTxtr;
			sampler2D _Sampler60426;
			sampler2D _RadialMask;
			sampler2D _RadialTxtr;
			sampler2D _Sampler60354;
			sampler2D _NebulaTxtr;
			sampler2D _CenterTxtr;
			sampler2D _AlphaTxtr;


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

			VertexOutput VertexFunction ( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord3.xy = v.ase_texcoord1.xy;
				o.ase_texcoord3.zw = v.ase_texcoord.xy;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = defaultVertexValue;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					o.worldPos = positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				#ifdef ASE_FOG
					o.fogFactor = ComputeFogFactor( positionCS.z );
				#endif

				o.clipPos = positionCS;

				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_texcoord1 = v.ase_texcoord1;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_texcoord1 = patch[0].ase_texcoord1 * bary.x + patch[1].ase_texcoord1 * bary.y + patch[2].ase_texcoord1 * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag ( VertexOutput IN  ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					float3 WorldPosition = IN.worldPos;
				#endif

				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float4 break44_g39 = _BorderTO;
				float4 appendResult41_g39 = (float4(break44_g39.x , break44_g39.y , 0.0 , 0.0));
				float4 break45_g39 = float4( 0,0,0,0 );
				float4 appendResult43_g39 = (float4(( break45_g39.z - _BorderMoveUv ) , break45_g39.w , 0.0 , 0.0));
				float2 texCoord51_g39 = IN.ase_texcoord3.xy * appendResult41_g39.xy + appendResult43_g39.xy;
				float2 panner46_g39 = ( ( _TimeParameters.x ) * _BorderSpeed + texCoord51_g39);
				float2 temp_output_1_0_g44 = float2( 2.83,1 );
				float2 texCoord80_g44 = IN.ase_texcoord3.zw * float2( 1,1 ) + float2( 0,0 );
				float2 appendResult10_g44 = (float2(( (temp_output_1_0_g44).x * texCoord80_g44.x ) , ( texCoord80_g44.y * (temp_output_1_0_g44).y )));
				float2 temp_output_11_0_g44 = float2( 0,0 );
				float2 texCoord81_g44 = IN.ase_texcoord3.zw * float2( 1,1 ) + float2( 0,0 );
				float2 panner18_g44 = ( ( (temp_output_11_0_g44).x * _TimeParameters.x ) * float2( 1,0 ) + texCoord81_g44);
				float2 panner19_g44 = ( ( _TimeParameters.x * (temp_output_11_0_g44).y ) * float2( 0,1 ) + texCoord81_g44);
				float2 appendResult24_g44 = (float2((panner18_g44).x , (panner19_g44).y));
				float2 temp_output_47_0_g44 = _RadialBorderTextureSpeed;
				float2 texCoord424 = IN.ase_texcoord3.xy * float2( 1,1 ) + _RadialBorderOffset;
				float2 temp_output_31_0_g44 = ( texCoord424 - float2( 1,1 ) );
				float2 appendResult39_g44 = (float2(frac( ( atan2( (temp_output_31_0_g44).x , (temp_output_31_0_g44).y ) / TWO_PI ) ) , length( temp_output_31_0_g44 )));
				float2 panner54_g44 = ( ( (temp_output_47_0_g44).x * _TimeParameters.x ) * float2( 1,0 ) + appendResult39_g44);
				float2 panner55_g44 = ( ( _TimeParameters.x * (temp_output_47_0_g44).y ) * float2( 0,1 ) + appendResult39_g44);
				float2 appendResult58_g44 = (float2((panner54_g44).x , (panner55_g44).y));
				float4 tex2DNode140 = tex2D( _BordersTxtr, (( _RadialUV )?( ( ( (tex2D( _Sampler60426, ( appendResult10_g44 + appendResult24_g44 ) )).rg * 1.0 ) + ( _RadialBorderTextureCoordination * appendResult58_g44 ) ) ):( panner46_g39 )) );
				float4 break44_g38 = _NoiseTO;
				float4 appendResult41_g38 = (float4(break44_g38.x , break44_g38.y , 0.0 , 0.0));
				float4 break45_g38 = float4( 0,0,0,0 );
				float4 appendResult43_g38 = (float4(( break45_g38.z - _NoiseUv ) , break45_g38.w , 0.0 , 0.0));
				float2 texCoord51_g38 = IN.ase_texcoord3.xy * appendResult41_g38.xy + appendResult43_g38.xy;
				float2 panner46_g38 = ( ( _TimeParameters.x ) * _NoiseSpeed + texCoord51_g38);
				float simplePerlin2D209 = snoise( panner46_g38*_BordersNoiseSize );
				simplePerlin2D209 = simplePerlin2D209*0.5 + 0.5;
				float4 temp_cast_4 = (simplePerlin2D209).xxxx;
				float4 clampResult213 = clamp( ( tex2DNode140 + ( tex2DNode140 - temp_cast_4 ) ) , float4( 0,0,0,0 ) , float4( 1,1,1,0 ) );
				float2 uv1_RadialMask409 = IN.ase_texcoord3.xy;
				float2 temp_output_1_0_g25 = float2( 2.83,1 );
				float2 texCoord80_g25 = IN.ase_texcoord3.zw * float2( 1,1 ) + float2( 0,0 );
				float2 appendResult10_g25 = (float2(( (temp_output_1_0_g25).x * texCoord80_g25.x ) , ( texCoord80_g25.y * (temp_output_1_0_g25).y )));
				float2 temp_output_11_0_g25 = float2( 0,0 );
				float2 texCoord81_g25 = IN.ase_texcoord3.zw * float2( 1,1 ) + float2( 0,0 );
				float2 panner18_g25 = ( ( (temp_output_11_0_g25).x * _TimeParameters.x ) * float2( 1,0 ) + texCoord81_g25);
				float2 panner19_g25 = ( ( _TimeParameters.x * (temp_output_11_0_g25).y ) * float2( 0,1 ) + texCoord81_g25);
				float2 appendResult24_g25 = (float2((panner18_g25).x , (panner19_g25).y));
				float2 temp_output_47_0_g25 = _RadialTextureSpeed;
				float2 texCoord397 = IN.ase_texcoord3.xy * _RadialTiling + _RadialOffset;
				float2 temp_output_31_0_g25 = ( texCoord397 - float2( 1,1 ) );
				float2 appendResult39_g25 = (float2(frac( ( atan2( (temp_output_31_0_g25).x , (temp_output_31_0_g25).y ) / TWO_PI ) ) , length( temp_output_31_0_g25 )));
				float2 panner54_g25 = ( ( (temp_output_47_0_g25).x * _TimeParameters.x ) * float2( 1,0 ) + appendResult39_g25);
				float2 panner55_g25 = ( ( _TimeParameters.x * (temp_output_47_0_g25).y ) * float2( 0,1 ) + appendResult39_g25);
				float2 appendResult58_g25 = (float2((panner54_g25).x , (panner55_g25).y));
				float4 tex2DNode304 = tex2D( _RadialTxtr, ( ( (tex2D( _Sampler60354, ( appendResult10_g25 + appendResult24_g25 ) )).rg * 1.0 ) + ( _RadialTextureCoordination * appendResult58_g25 ) ) );
				float4 break44_g31 = _RadialNoiseTO;
				float4 appendResult41_g31 = (float4(break44_g31.x , break44_g31.y , 0.0 , 0.0));
				float4 break45_g31 = float4( 0,0,0,0 );
				float4 appendResult43_g31 = (float4(( break45_g31.z - _RadialNoiseMoveUV ) , break45_g31.w , 0.0 , 0.0));
				float2 texCoord51_g31 = IN.ase_texcoord3.xy * appendResult41_g31.xy + appendResult43_g31.xy;
				float2 panner46_g31 = ( ( _TimeParameters.x ) * _RadialNoiseSpeed + texCoord51_g31);
				float simplePerlin2D301 = snoise( panner46_g31*_RadialNoiseSize );
				simplePerlin2D301 = simplePerlin2D301*0.5 + 0.5;
				float4 temp_cast_7 = (simplePerlin2D301).xxxx;
				float4 clampResult300 = clamp( ( tex2DNode304 + ( tex2DNode304 - temp_cast_7 ) ) , float4( 0,0,0,0 ) , float4( 1,1,1,0 ) );
				float4 break44_g34 = _NebulaTO;
				float4 appendResult41_g34 = (float4(break44_g34.x , break44_g34.y , 0.0 , 0.0));
				float4 break45_g34 = float4( 0,0,0,0 );
				float4 appendResult43_g34 = (float4(( break45_g34.z - _NebulaMoveUv ) , break45_g34.w , 0.0 , 0.0));
				float2 texCoord51_g34 = IN.ase_texcoord3.xy * appendResult41_g34.xy + appendResult43_g34.xy;
				float2 panner46_g34 = ( ( _TimeParameters.x ) * _NebulaSpeed + texCoord51_g34);
				float4 break44_g37 = _CenterTxtrTO1;
				float4 appendResult41_g37 = (float4(break44_g37.x , break44_g37.y , 0.0 , 0.0));
				float4 break45_g37 = float4( 0,0,0,0 );
				float4 appendResult43_g37 = (float4(( break45_g37.z - _CenterTxtrMoveUV1 ) , break45_g37.w , 0.0 , 0.0));
				float2 texCoord51_g37 = IN.ase_texcoord3.xy * appendResult41_g37.xy + appendResult43_g37.xy;
				float2 panner46_g37 = ( ( _TimeParameters.x ) * _CenterTxtrSpeed1 + texCoord51_g37);
				float4 tex2DNode31 = tex2D( _CenterTxtr, panner46_g37 );
				float4 break44_g36 = _CenterNoiseTO;
				float4 appendResult41_g36 = (float4(break44_g36.x , break44_g36.y , 0.0 , 0.0));
				float4 break45_g36 = float4( 0,0,0,0 );
				float4 appendResult43_g36 = (float4(( break45_g36.z - _CenterNoiseMoveUV ) , break45_g36.w , 0.0 , 0.0));
				float2 texCoord51_g36 = IN.ase_texcoord3.xy * appendResult41_g36.xy + appendResult43_g36.xy;
				float2 panner46_g36 = ( ( _TimeParameters.x ) * _CenterNoiseSpeed + texCoord51_g36);
				float simplePerlin2D233 = snoise( panner46_g36*_CenterNoiseSize );
				simplePerlin2D233 = simplePerlin2D233*0.5 + 0.5;
				float4 temp_cast_14 = (simplePerlin2D233).xxxx;
				float4 clampResult231 = clamp( ( tex2DNode31 + ( tex2DNode31 - temp_cast_14 ) ) , float4( 0,0,0,0 ) , float4( 1,1,1,0 ) );
				
				float4 break44_g32 = _AlphaTextureTO;
				float4 appendResult41_g32 = (float4(break44_g32.x , break44_g32.y , 0.0 , 0.0));
				float4 break45_g32 = float4( 0,0,0,0 );
				float4 appendResult43_g32 = (float4(( break45_g32.z - _AlphaTextureMoveUv ) , break45_g32.w , 0.0 , 0.0));
				float2 texCoord51_g32 = IN.ase_texcoord3.xy * appendResult41_g32.xy + appendResult43_g32.xy;
				float2 panner46_g32 = ( ( _TimeParameters.x ) * _AlphaTextureSpeed + texCoord51_g32);
				float4 tex2DNode74 = tex2D( _AlphaTxtr, panner46_g32 );
				float4 break44_g33 = _AlphaNTO;
				float4 appendResult41_g33 = (float4(break44_g33.x , break44_g33.y , 0.0 , 0.0));
				float4 break45_g33 = float4( 0,0,0,0 );
				float4 appendResult43_g33 = (float4(( break45_g33.z - _AlphaNMoveUv ) , break45_g33.w , 0.0 , 0.0));
				float2 texCoord51_g33 = IN.ase_texcoord3.xy * appendResult41_g33.xy + appendResult43_g33.xy;
				float2 panner46_g33 = ( ( _TimeParameters.x ) * _AlphaNSpeed + texCoord51_g33);
				float simplePerlin2D195 = snoise( panner46_g33*_AlphaNoiseSize );
				simplePerlin2D195 = simplePerlin2D195*0.5 + 0.5;
				float4 temp_cast_20 = (simplePerlin2D195).xxxx;
				float4 clampResult194 = clamp( ( tex2DNode74 + ( tex2DNode74 - temp_cast_20 ) ) , float4( 0,0,0,0 ) , float4( 1,1,1,0 ) );
				
				float3 BakedAlbedo = 0;
				float3 BakedEmission = 0;
				float3 Color = ( ( _BordersColor * clampResult213 ) + ( ( ( 1.0 - tex2D( _RadialMask, uv1_RadialMask409 ) ) * ( clampResult300 * _RadialTxtrColors ) ) + ( ( tex2D( _NebulaTxtr, panner46_g34 ) * _NebulaColor ) + ( clampResult231 * _CenterTxtrColors ) ) ) ).rgb;
				float Alpha = clampResult194.r;
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;

				#ifdef _ALPHATEST_ON
					clip( Alpha - AlphaClipThreshold );
				#endif

				#if defined(_DBUFFER)
					ApplyDecalToBaseColor(IN.clipPos, Color);
				#endif

				#if defined(_ALPHAPREMULTIPLY_ON)
				Color *= Alpha;
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				#ifdef ASE_FOG
					Color = MixFog( Color, IN.fogFactor );
				#endif

				return half4( Color, Alpha );
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "ShadowCaster"
			Tags { "LightMode"="ShadowCaster" }

			ZWrite On
			ZTest LEqual
			AlphaToMask Off
			ColorMask 0

			HLSLPROGRAM

			#pragma multi_compile_instancing
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_SRP_VERSION 120108


			#pragma vertex vert
			#pragma fragment frag

			#pragma multi_compile _ _CASTING_PUNCTUAL_LIGHT_SHADOW

			#define SHADERPASS SHADERPASS_SHADOWCASTER

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BordersColor;
			float4 _CenterTxtrTO1;
			float4 _NebulaColor;
			float4 _NebulaTO;
			float4 _RadialTxtrColors;
			float4 _CenterTxtrColors;
			float4 _RadialNoiseTO;
			float4 _AlphaTextureTO;
			float4 _NoiseTO;
			float4 _CenterNoiseTO;
			float4 _AlphaNTO;
			float4 _BorderTO;
			float2 _RadialBorderTextureCoordination;
			float2 _CenterTxtrSpeed1;
			float2 _BorderSpeed;
			float2 _NebulaSpeed;
			float2 _AlphaNSpeed;
			float2 _NoiseSpeed;
			float2 _RadialTiling;
			float2 _RadialBorderTextureSpeed;
			float2 _RadialNoiseSpeed;
			float2 _RadialOffset;
			float2 _CenterNoiseSpeed;
			float2 _RadialTextureSpeed;
			float2 _RadialTextureCoordination;
			float2 _RadialBorderOffset;
			float2 _AlphaTextureSpeed;
			float _CenterNoiseSize;
			float _CenterNoiseMoveUV;
			float _AlphaTextureMoveUv;
			float _RadialNoiseSize;
			float _NebulaMoveUv;
			float _AlphaNMoveUv;
			float _RadialNoiseMoveUV;
			float _BordersNoiseSize;
			float _NoiseUv;
			float _BorderMoveUv;
			float _RadialUV;
			float _CenterTxtrMoveUV1;
			float _AlphaNoiseSize;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _AlphaTxtr;


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

			float3 _LightDirection;
			float3 _LightPosition;

			VertexOutput VertexFunction( VertexInput v )
			{
				VertexOutput o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				o.ase_texcoord2.xy = v.ase_texcoord1.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = defaultVertexValue;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					o.worldPos = positionWS;
				#endif

				float3 normalWS = TransformObjectToWorldDir( v.ase_normal );

				#if _CASTING_PUNCTUAL_LIGHT_SHADOW
					float3 lightDirectionWS = normalize(_LightPosition - positionWS);
				#else
					float3 lightDirectionWS = _LightDirection;
				#endif

				float4 clipPos = TransformWorldToHClip(ApplyShadowBias(positionWS, normalWS, lightDirectionWS));

				#if UNITY_REVERSED_Z
					clipPos.z = min(clipPos.z, UNITY_NEAR_CLIP_VALUE);
				#else
					clipPos.z = max(clipPos.z, UNITY_NEAR_CLIP_VALUE);
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				o.clipPos = clipPos;

				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_texcoord1 = v.ase_texcoord1;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_texcoord1 = patch[0].ase_texcoord1 * bary.x + patch[1].ase_texcoord1 * bary.y + patch[2].ase_texcoord1 * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					float3 WorldPosition = IN.worldPos;
				#endif

				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float4 break44_g32 = _AlphaTextureTO;
				float4 appendResult41_g32 = (float4(break44_g32.x , break44_g32.y , 0.0 , 0.0));
				float4 break45_g32 = float4( 0,0,0,0 );
				float4 appendResult43_g32 = (float4(( break45_g32.z - _AlphaTextureMoveUv ) , break45_g32.w , 0.0 , 0.0));
				float2 texCoord51_g32 = IN.ase_texcoord2.xy * appendResult41_g32.xy + appendResult43_g32.xy;
				float2 panner46_g32 = ( ( _TimeParameters.x ) * _AlphaTextureSpeed + texCoord51_g32);
				float4 tex2DNode74 = tex2D( _AlphaTxtr, panner46_g32 );
				float4 break44_g33 = _AlphaNTO;
				float4 appendResult41_g33 = (float4(break44_g33.x , break44_g33.y , 0.0 , 0.0));
				float4 break45_g33 = float4( 0,0,0,0 );
				float4 appendResult43_g33 = (float4(( break45_g33.z - _AlphaNMoveUv ) , break45_g33.w , 0.0 , 0.0));
				float2 texCoord51_g33 = IN.ase_texcoord2.xy * appendResult41_g33.xy + appendResult43_g33.xy;
				float2 panner46_g33 = ( ( _TimeParameters.x ) * _AlphaNSpeed + texCoord51_g33);
				float simplePerlin2D195 = snoise( panner46_g33*_AlphaNoiseSize );
				simplePerlin2D195 = simplePerlin2D195*0.5 + 0.5;
				float4 temp_cast_4 = (simplePerlin2D195).xxxx;
				float4 clampResult194 = clamp( ( tex2DNode74 + ( tex2DNode74 - temp_cast_4 ) ) , float4( 0,0,0,0 ) , float4( 1,1,1,0 ) );
				

				float Alpha = clampResult194.r;
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;

				#ifdef _ALPHATEST_ON
					#ifdef _ALPHATEST_SHADOW_ON
						clip(Alpha - AlphaClipThresholdShadow);
					#else
						clip(Alpha - AlphaClipThreshold);
					#endif
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				return 0;
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthOnly"
			Tags { "LightMode"="DepthOnly" }

			ZWrite On
			ColorMask 0
			AlphaToMask Off

			HLSLPROGRAM

			#pragma multi_compile_instancing
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_SRP_VERSION 120108


			#pragma vertex vert
			#pragma fragment frag

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BordersColor;
			float4 _CenterTxtrTO1;
			float4 _NebulaColor;
			float4 _NebulaTO;
			float4 _RadialTxtrColors;
			float4 _CenterTxtrColors;
			float4 _RadialNoiseTO;
			float4 _AlphaTextureTO;
			float4 _NoiseTO;
			float4 _CenterNoiseTO;
			float4 _AlphaNTO;
			float4 _BorderTO;
			float2 _RadialBorderTextureCoordination;
			float2 _CenterTxtrSpeed1;
			float2 _BorderSpeed;
			float2 _NebulaSpeed;
			float2 _AlphaNSpeed;
			float2 _NoiseSpeed;
			float2 _RadialTiling;
			float2 _RadialBorderTextureSpeed;
			float2 _RadialNoiseSpeed;
			float2 _RadialOffset;
			float2 _CenterNoiseSpeed;
			float2 _RadialTextureSpeed;
			float2 _RadialTextureCoordination;
			float2 _RadialBorderOffset;
			float2 _AlphaTextureSpeed;
			float _CenterNoiseSize;
			float _CenterNoiseMoveUV;
			float _AlphaTextureMoveUv;
			float _RadialNoiseSize;
			float _NebulaMoveUv;
			float _AlphaNMoveUv;
			float _RadialNoiseMoveUV;
			float _BordersNoiseSize;
			float _NoiseUv;
			float _BorderMoveUv;
			float _RadialUV;
			float _CenterTxtrMoveUV1;
			float _AlphaNoiseSize;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _AlphaTxtr;


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord2.xy = v.ase_texcoord1.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = defaultVertexValue;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					o.worldPos = positionWS;
				#endif

				o.clipPos = TransformWorldToHClip( positionWS );
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = o.clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_texcoord1 = v.ase_texcoord1;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_texcoord1 = patch[0].ase_texcoord1 * bary.x + patch[1].ase_texcoord1 * bary.y + patch[2].ase_texcoord1 * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					float3 WorldPosition = IN.worldPos;
				#endif

				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float4 break44_g32 = _AlphaTextureTO;
				float4 appendResult41_g32 = (float4(break44_g32.x , break44_g32.y , 0.0 , 0.0));
				float4 break45_g32 = float4( 0,0,0,0 );
				float4 appendResult43_g32 = (float4(( break45_g32.z - _AlphaTextureMoveUv ) , break45_g32.w , 0.0 , 0.0));
				float2 texCoord51_g32 = IN.ase_texcoord2.xy * appendResult41_g32.xy + appendResult43_g32.xy;
				float2 panner46_g32 = ( ( _TimeParameters.x ) * _AlphaTextureSpeed + texCoord51_g32);
				float4 tex2DNode74 = tex2D( _AlphaTxtr, panner46_g32 );
				float4 break44_g33 = _AlphaNTO;
				float4 appendResult41_g33 = (float4(break44_g33.x , break44_g33.y , 0.0 , 0.0));
				float4 break45_g33 = float4( 0,0,0,0 );
				float4 appendResult43_g33 = (float4(( break45_g33.z - _AlphaNMoveUv ) , break45_g33.w , 0.0 , 0.0));
				float2 texCoord51_g33 = IN.ase_texcoord2.xy * appendResult41_g33.xy + appendResult43_g33.xy;
				float2 panner46_g33 = ( ( _TimeParameters.x ) * _AlphaNSpeed + texCoord51_g33);
				float simplePerlin2D195 = snoise( panner46_g33*_AlphaNoiseSize );
				simplePerlin2D195 = simplePerlin2D195*0.5 + 0.5;
				float4 temp_cast_4 = (simplePerlin2D195).xxxx;
				float4 clampResult194 = clamp( ( tex2DNode74 + ( tex2DNode74 - temp_cast_4 ) ) , float4( 0,0,0,0 ) , float4( 1,1,1,0 ) );
				

				float Alpha = clampResult194.r;
				float AlphaClipThreshold = 0.5;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				return 0;
			}
			ENDHLSL
		}

		
		Pass
		{
			
            Name "SceneSelectionPass"
            Tags { "LightMode"="SceneSelectionPass" }

			Cull Off

			HLSLPROGRAM

			#pragma multi_compile_instancing
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_SRP_VERSION 120108


			#pragma vertex vert
			#pragma fragment frag

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT
			#define SHADERPASS SHADERPASS_DEPTHONLY

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BordersColor;
			float4 _CenterTxtrTO1;
			float4 _NebulaColor;
			float4 _NebulaTO;
			float4 _RadialTxtrColors;
			float4 _CenterTxtrColors;
			float4 _RadialNoiseTO;
			float4 _AlphaTextureTO;
			float4 _NoiseTO;
			float4 _CenterNoiseTO;
			float4 _AlphaNTO;
			float4 _BorderTO;
			float2 _RadialBorderTextureCoordination;
			float2 _CenterTxtrSpeed1;
			float2 _BorderSpeed;
			float2 _NebulaSpeed;
			float2 _AlphaNSpeed;
			float2 _NoiseSpeed;
			float2 _RadialTiling;
			float2 _RadialBorderTextureSpeed;
			float2 _RadialNoiseSpeed;
			float2 _RadialOffset;
			float2 _CenterNoiseSpeed;
			float2 _RadialTextureSpeed;
			float2 _RadialTextureCoordination;
			float2 _RadialBorderOffset;
			float2 _AlphaTextureSpeed;
			float _CenterNoiseSize;
			float _CenterNoiseMoveUV;
			float _AlphaTextureMoveUv;
			float _RadialNoiseSize;
			float _NebulaMoveUv;
			float _AlphaNMoveUv;
			float _RadialNoiseMoveUV;
			float _BordersNoiseSize;
			float _NoiseUv;
			float _BorderMoveUv;
			float _RadialUV;
			float _CenterTxtrMoveUV1;
			float _AlphaNoiseSize;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _AlphaTxtr;


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

			int _ObjectId;
			int _PassValue;

			struct SurfaceDescription
			{
				float Alpha;
				float AlphaClipThreshold;
			};

			VertexOutput VertexFunction(VertexInput v  )
			{
				VertexOutput o;
				ZERO_INITIALIZE(VertexOutput, o);

				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord.xy = v.ase_texcoord1.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord.zw = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = defaultVertexValue;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				o.clipPos = TransformWorldToHClip(positionWS);

				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_texcoord1 = v.ase_texcoord1;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_texcoord1 = patch[0].ase_texcoord1 * bary.x + patch[1].ase_texcoord1 * bary.y + patch[2].ase_texcoord1 * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN ) : SV_TARGET
			{
				SurfaceDescription surfaceDescription = (SurfaceDescription)0;

				float4 break44_g32 = _AlphaTextureTO;
				float4 appendResult41_g32 = (float4(break44_g32.x , break44_g32.y , 0.0 , 0.0));
				float4 break45_g32 = float4( 0,0,0,0 );
				float4 appendResult43_g32 = (float4(( break45_g32.z - _AlphaTextureMoveUv ) , break45_g32.w , 0.0 , 0.0));
				float2 texCoord51_g32 = IN.ase_texcoord.xy * appendResult41_g32.xy + appendResult43_g32.xy;
				float2 panner46_g32 = ( ( _TimeParameters.x ) * _AlphaTextureSpeed + texCoord51_g32);
				float4 tex2DNode74 = tex2D( _AlphaTxtr, panner46_g32 );
				float4 break44_g33 = _AlphaNTO;
				float4 appendResult41_g33 = (float4(break44_g33.x , break44_g33.y , 0.0 , 0.0));
				float4 break45_g33 = float4( 0,0,0,0 );
				float4 appendResult43_g33 = (float4(( break45_g33.z - _AlphaNMoveUv ) , break45_g33.w , 0.0 , 0.0));
				float2 texCoord51_g33 = IN.ase_texcoord.xy * appendResult41_g33.xy + appendResult43_g33.xy;
				float2 panner46_g33 = ( ( _TimeParameters.x ) * _AlphaNSpeed + texCoord51_g33);
				float simplePerlin2D195 = snoise( panner46_g33*_AlphaNoiseSize );
				simplePerlin2D195 = simplePerlin2D195*0.5 + 0.5;
				float4 temp_cast_4 = (simplePerlin2D195).xxxx;
				float4 clampResult194 = clamp( ( tex2DNode74 + ( tex2DNode74 - temp_cast_4 ) ) , float4( 0,0,0,0 ) , float4( 1,1,1,0 ) );
				

				surfaceDescription.Alpha = clampResult194.r;
				surfaceDescription.AlphaClipThreshold = 0.5;

				#if _ALPHATEST_ON
					float alphaClipThreshold = 0.01f;
					#if ALPHA_CLIP_THRESHOLD
						alphaClipThreshold = surfaceDescription.AlphaClipThreshold;
					#endif
					clip(surfaceDescription.Alpha - alphaClipThreshold);
				#endif

				half4 outColor = half4(_ObjectId, _PassValue, 1.0, 1.0);
				return outColor;
			}
			ENDHLSL
		}

		
		Pass
		{
			
            Name "ScenePickingPass"
            Tags { "LightMode"="Picking" }

			HLSLPROGRAM

			#pragma multi_compile_instancing
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_SRP_VERSION 120108


			#pragma vertex vert
			#pragma fragment frag

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT
			#define SHADERPASS SHADERPASS_DEPTHONLY

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BordersColor;
			float4 _CenterTxtrTO1;
			float4 _NebulaColor;
			float4 _NebulaTO;
			float4 _RadialTxtrColors;
			float4 _CenterTxtrColors;
			float4 _RadialNoiseTO;
			float4 _AlphaTextureTO;
			float4 _NoiseTO;
			float4 _CenterNoiseTO;
			float4 _AlphaNTO;
			float4 _BorderTO;
			float2 _RadialBorderTextureCoordination;
			float2 _CenterTxtrSpeed1;
			float2 _BorderSpeed;
			float2 _NebulaSpeed;
			float2 _AlphaNSpeed;
			float2 _NoiseSpeed;
			float2 _RadialTiling;
			float2 _RadialBorderTextureSpeed;
			float2 _RadialNoiseSpeed;
			float2 _RadialOffset;
			float2 _CenterNoiseSpeed;
			float2 _RadialTextureSpeed;
			float2 _RadialTextureCoordination;
			float2 _RadialBorderOffset;
			float2 _AlphaTextureSpeed;
			float _CenterNoiseSize;
			float _CenterNoiseMoveUV;
			float _AlphaTextureMoveUv;
			float _RadialNoiseSize;
			float _NebulaMoveUv;
			float _AlphaNMoveUv;
			float _RadialNoiseMoveUV;
			float _BordersNoiseSize;
			float _NoiseUv;
			float _BorderMoveUv;
			float _RadialUV;
			float _CenterTxtrMoveUV1;
			float _AlphaNoiseSize;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _AlphaTxtr;


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

			float4 _SelectionID;


			struct SurfaceDescription
			{
				float Alpha;
				float AlphaClipThreshold;
			};

			VertexOutput VertexFunction(VertexInput v  )
			{
				VertexOutput o;
				ZERO_INITIALIZE(VertexOutput, o);

				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord.xy = v.ase_texcoord1.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif
				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				o.clipPos = TransformWorldToHClip(positionWS);
				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_texcoord1 = v.ase_texcoord1;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_texcoord1 = patch[0].ase_texcoord1 * bary.x + patch[1].ase_texcoord1 * bary.y + patch[2].ase_texcoord1 * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN ) : SV_TARGET
			{
				SurfaceDescription surfaceDescription = (SurfaceDescription)0;

				float4 break44_g32 = _AlphaTextureTO;
				float4 appendResult41_g32 = (float4(break44_g32.x , break44_g32.y , 0.0 , 0.0));
				float4 break45_g32 = float4( 0,0,0,0 );
				float4 appendResult43_g32 = (float4(( break45_g32.z - _AlphaTextureMoveUv ) , break45_g32.w , 0.0 , 0.0));
				float2 texCoord51_g32 = IN.ase_texcoord.xy * appendResult41_g32.xy + appendResult43_g32.xy;
				float2 panner46_g32 = ( ( _TimeParameters.x ) * _AlphaTextureSpeed + texCoord51_g32);
				float4 tex2DNode74 = tex2D( _AlphaTxtr, panner46_g32 );
				float4 break44_g33 = _AlphaNTO;
				float4 appendResult41_g33 = (float4(break44_g33.x , break44_g33.y , 0.0 , 0.0));
				float4 break45_g33 = float4( 0,0,0,0 );
				float4 appendResult43_g33 = (float4(( break45_g33.z - _AlphaNMoveUv ) , break45_g33.w , 0.0 , 0.0));
				float2 texCoord51_g33 = IN.ase_texcoord.xy * appendResult41_g33.xy + appendResult43_g33.xy;
				float2 panner46_g33 = ( ( _TimeParameters.x ) * _AlphaNSpeed + texCoord51_g33);
				float simplePerlin2D195 = snoise( panner46_g33*_AlphaNoiseSize );
				simplePerlin2D195 = simplePerlin2D195*0.5 + 0.5;
				float4 temp_cast_4 = (simplePerlin2D195).xxxx;
				float4 clampResult194 = clamp( ( tex2DNode74 + ( tex2DNode74 - temp_cast_4 ) ) , float4( 0,0,0,0 ) , float4( 1,1,1,0 ) );
				

				surfaceDescription.Alpha = clampResult194.r;
				surfaceDescription.AlphaClipThreshold = 0.5;

				#if _ALPHATEST_ON
					float alphaClipThreshold = 0.01f;
					#if ALPHA_CLIP_THRESHOLD
						alphaClipThreshold = surfaceDescription.AlphaClipThreshold;
					#endif
					clip(surfaceDescription.Alpha - alphaClipThreshold);
				#endif

				half4 outColor = 0;
				outColor = _SelectionID;

				return outColor;
			}

			ENDHLSL
		}

		
		Pass
		{
			
            Name "DepthNormals"
            Tags { "LightMode"="DepthNormalsOnly" }

			ZTest LEqual
			ZWrite On


			HLSLPROGRAM

			#pragma multi_compile_instancing
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_SRP_VERSION 120108


			#pragma vertex vert
			#pragma fragment frag

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT
			#define VARYINGS_NEED_NORMAL_WS

			#define SHADERPASS SHADERPASS_DEPTHNORMALSONLY

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float3 normalWS : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BordersColor;
			float4 _CenterTxtrTO1;
			float4 _NebulaColor;
			float4 _NebulaTO;
			float4 _RadialTxtrColors;
			float4 _CenterTxtrColors;
			float4 _RadialNoiseTO;
			float4 _AlphaTextureTO;
			float4 _NoiseTO;
			float4 _CenterNoiseTO;
			float4 _AlphaNTO;
			float4 _BorderTO;
			float2 _RadialBorderTextureCoordination;
			float2 _CenterTxtrSpeed1;
			float2 _BorderSpeed;
			float2 _NebulaSpeed;
			float2 _AlphaNSpeed;
			float2 _NoiseSpeed;
			float2 _RadialTiling;
			float2 _RadialBorderTextureSpeed;
			float2 _RadialNoiseSpeed;
			float2 _RadialOffset;
			float2 _CenterNoiseSpeed;
			float2 _RadialTextureSpeed;
			float2 _RadialTextureCoordination;
			float2 _RadialBorderOffset;
			float2 _AlphaTextureSpeed;
			float _CenterNoiseSize;
			float _CenterNoiseMoveUV;
			float _AlphaTextureMoveUv;
			float _RadialNoiseSize;
			float _NebulaMoveUv;
			float _AlphaNMoveUv;
			float _RadialNoiseMoveUV;
			float _BordersNoiseSize;
			float _NoiseUv;
			float _BorderMoveUv;
			float _RadialUV;
			float _CenterTxtrMoveUV1;
			float _AlphaNoiseSize;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _AlphaTxtr;


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

			struct SurfaceDescription
			{
				float Alpha;
				float AlphaClipThreshold;
			};

			VertexOutput VertexFunction(VertexInput v  )
			{
				VertexOutput o;
				ZERO_INITIALIZE(VertexOutput, o);

				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord1.xy = v.ase_texcoord1.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord1.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = defaultVertexValue;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 normalWS = TransformObjectToWorldNormal(v.ase_normal);

				o.clipPos = TransformWorldToHClip(positionWS);
				o.normalWS.xyz =  normalWS;

				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_texcoord1 = v.ase_texcoord1;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_texcoord1 = patch[0].ase_texcoord1 * bary.x + patch[1].ase_texcoord1 * bary.y + patch[2].ase_texcoord1 * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN ) : SV_TARGET
			{
				SurfaceDescription surfaceDescription = (SurfaceDescription)0;

				float4 break44_g32 = _AlphaTextureTO;
				float4 appendResult41_g32 = (float4(break44_g32.x , break44_g32.y , 0.0 , 0.0));
				float4 break45_g32 = float4( 0,0,0,0 );
				float4 appendResult43_g32 = (float4(( break45_g32.z - _AlphaTextureMoveUv ) , break45_g32.w , 0.0 , 0.0));
				float2 texCoord51_g32 = IN.ase_texcoord1.xy * appendResult41_g32.xy + appendResult43_g32.xy;
				float2 panner46_g32 = ( ( _TimeParameters.x ) * _AlphaTextureSpeed + texCoord51_g32);
				float4 tex2DNode74 = tex2D( _AlphaTxtr, panner46_g32 );
				float4 break44_g33 = _AlphaNTO;
				float4 appendResult41_g33 = (float4(break44_g33.x , break44_g33.y , 0.0 , 0.0));
				float4 break45_g33 = float4( 0,0,0,0 );
				float4 appendResult43_g33 = (float4(( break45_g33.z - _AlphaNMoveUv ) , break45_g33.w , 0.0 , 0.0));
				float2 texCoord51_g33 = IN.ase_texcoord1.xy * appendResult41_g33.xy + appendResult43_g33.xy;
				float2 panner46_g33 = ( ( _TimeParameters.x ) * _AlphaNSpeed + texCoord51_g33);
				float simplePerlin2D195 = snoise( panner46_g33*_AlphaNoiseSize );
				simplePerlin2D195 = simplePerlin2D195*0.5 + 0.5;
				float4 temp_cast_4 = (simplePerlin2D195).xxxx;
				float4 clampResult194 = clamp( ( tex2DNode74 + ( tex2DNode74 - temp_cast_4 ) ) , float4( 0,0,0,0 ) , float4( 1,1,1,0 ) );
				

				surfaceDescription.Alpha = clampResult194.r;
				surfaceDescription.AlphaClipThreshold = 0.5;

				#if _ALPHATEST_ON
					clip(surfaceDescription.Alpha - surfaceDescription.AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				float3 normalWS = IN.normalWS;

				return half4(NormalizeNormalPerPixel(normalWS), 0.0);
			}

			ENDHLSL
		}

		
		Pass
		{
			
            Name "DepthNormalsOnly"
            Tags { "LightMode"="DepthNormalsOnly" }

			ZTest LEqual
			ZWrite On

			HLSLPROGRAM

			#pragma multi_compile_instancing
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_SRP_VERSION 120108


			#pragma exclude_renderers glcore gles gles3 
			#pragma vertex vert
			#pragma fragment frag

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT
			#define ATTRIBUTES_NEED_TEXCOORD1
			#define VARYINGS_NEED_NORMAL_WS
			#define VARYINGS_NEED_TANGENT_WS

			#define SHADERPASS SHADERPASS_DEPTHNORMALSONLY

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float3 normalWS : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BordersColor;
			float4 _CenterTxtrTO1;
			float4 _NebulaColor;
			float4 _NebulaTO;
			float4 _RadialTxtrColors;
			float4 _CenterTxtrColors;
			float4 _RadialNoiseTO;
			float4 _AlphaTextureTO;
			float4 _NoiseTO;
			float4 _CenterNoiseTO;
			float4 _AlphaNTO;
			float4 _BorderTO;
			float2 _RadialBorderTextureCoordination;
			float2 _CenterTxtrSpeed1;
			float2 _BorderSpeed;
			float2 _NebulaSpeed;
			float2 _AlphaNSpeed;
			float2 _NoiseSpeed;
			float2 _RadialTiling;
			float2 _RadialBorderTextureSpeed;
			float2 _RadialNoiseSpeed;
			float2 _RadialOffset;
			float2 _CenterNoiseSpeed;
			float2 _RadialTextureSpeed;
			float2 _RadialTextureCoordination;
			float2 _RadialBorderOffset;
			float2 _AlphaTextureSpeed;
			float _CenterNoiseSize;
			float _CenterNoiseMoveUV;
			float _AlphaTextureMoveUv;
			float _RadialNoiseSize;
			float _NebulaMoveUv;
			float _AlphaNMoveUv;
			float _RadialNoiseMoveUV;
			float _BordersNoiseSize;
			float _NoiseUv;
			float _BorderMoveUv;
			float _RadialUV;
			float _CenterTxtrMoveUV1;
			float _AlphaNoiseSize;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _AlphaTxtr;


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

			struct SurfaceDescription
			{
				float Alpha;
				float AlphaClipThreshold;
			};

			VertexOutput VertexFunction(VertexInput v  )
			{
				VertexOutput o;
				ZERO_INITIALIZE(VertexOutput, o);

				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord1.xy = v.ase_texcoord1.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord1.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = defaultVertexValue;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 normalWS = TransformObjectToWorldNormal(v.ase_normal);

				o.clipPos = TransformWorldToHClip(positionWS);
				o.normalWS.xyz =  normalWS;

				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_texcoord1 = v.ase_texcoord1;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_texcoord1 = patch[0].ase_texcoord1 * bary.x + patch[1].ase_texcoord1 * bary.y + patch[2].ase_texcoord1 * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN ) : SV_TARGET
			{
				SurfaceDescription surfaceDescription = (SurfaceDescription)0;

				float4 break44_g32 = _AlphaTextureTO;
				float4 appendResult41_g32 = (float4(break44_g32.x , break44_g32.y , 0.0 , 0.0));
				float4 break45_g32 = float4( 0,0,0,0 );
				float4 appendResult43_g32 = (float4(( break45_g32.z - _AlphaTextureMoveUv ) , break45_g32.w , 0.0 , 0.0));
				float2 texCoord51_g32 = IN.ase_texcoord1.xy * appendResult41_g32.xy + appendResult43_g32.xy;
				float2 panner46_g32 = ( ( _TimeParameters.x ) * _AlphaTextureSpeed + texCoord51_g32);
				float4 tex2DNode74 = tex2D( _AlphaTxtr, panner46_g32 );
				float4 break44_g33 = _AlphaNTO;
				float4 appendResult41_g33 = (float4(break44_g33.x , break44_g33.y , 0.0 , 0.0));
				float4 break45_g33 = float4( 0,0,0,0 );
				float4 appendResult43_g33 = (float4(( break45_g33.z - _AlphaNMoveUv ) , break45_g33.w , 0.0 , 0.0));
				float2 texCoord51_g33 = IN.ase_texcoord1.xy * appendResult41_g33.xy + appendResult43_g33.xy;
				float2 panner46_g33 = ( ( _TimeParameters.x ) * _AlphaNSpeed + texCoord51_g33);
				float simplePerlin2D195 = snoise( panner46_g33*_AlphaNoiseSize );
				simplePerlin2D195 = simplePerlin2D195*0.5 + 0.5;
				float4 temp_cast_4 = (simplePerlin2D195).xxxx;
				float4 clampResult194 = clamp( ( tex2DNode74 + ( tex2DNode74 - temp_cast_4 ) ) , float4( 0,0,0,0 ) , float4( 1,1,1,0 ) );
				

				surfaceDescription.Alpha = clampResult194.r;
				surfaceDescription.AlphaClipThreshold = 0.5;

				#if _ALPHATEST_ON
					clip(surfaceDescription.Alpha - surfaceDescription.AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				float3 normalWS = IN.normalWS;

				return half4(NormalizeNormalPerPixel(normalWS), 0.0);
			}

			ENDHLSL
		}
		
	}
	
	CustomEditor "UnityEditor.ShaderGraphUnlitGUI"
	FallBack "Hidden/Shader Graph/FallbackError"
	
	Fallback Off
}
/*ASEBEGIN
Version=19103
Node;AmplifyShaderEditor.CommentaryNode;428;4297.222,-2014.987;Inherit;False;1199.348;683.1576;Comment;6;424;427;425;423;422;426;RadialUV;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;402;1901.339,456.378;Inherit;False;3030.776;957.55;Comment;21;299;300;301;302;303;304;328;329;326;325;363;327;354;397;398;399;400;404;409;416;415;RadialTxtr;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;381;4356.533,-1344.689;Inherit;False;2503.996;1048.526;Comment;16;212;213;211;208;209;139;375;138;140;376;291;289;290;137;142;143;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;379;5152.061,354.7227;Inherit;False;2263.732;922.6971;Comment;14;185;186;194;74;171;195;98;94;371;373;364;99;374;365;Alpha;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;378;166.5742,483.7419;Inherit;False;1602.177;805.8257;Comment;11;122;104;119;118;114;258;259;105;48;366;405;NebulaBG;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;377;-607.7307,-976.4739;Inherit;False;2346.672;1133.798;Comment;17;50;230;231;233;234;85;235;31;248;247;249;38;39;370;368;369;86;Center Txtr;1,1,1,1;0;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;52;-410,202;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;3;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=ShadowCaster;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;53;-410,202;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;3;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;False;False;True;1;LightMode=DepthOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;54;-410,202;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;3;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;55;-410,202;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;3;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=Universal2D;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;56;-410,202;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;SceneSelectionPass;0;6;SceneSelectionPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;3;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=SceneSelectionPass;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;57;-410,202;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ScenePickingPass;0;7;ScenePickingPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;3;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Picking;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;58;-410,202;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormals;0;8;DepthNormals;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;3;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;59;-410,202;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormalsOnly;0;9;DepthNormalsOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;3;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;True;9;d3d11;metal;vulkan;xboxone;xboxseries;playstation;ps4;ps5;switch;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;50;435.0911,-124.2307;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;3;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;0;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.SimpleAddOpNode;230;1005.641,-481.3024;Inherit;True;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;234;723.5647,-314.2896;Inherit;True;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.ColorNode;85;1218.225,-839.3054;Inherit;False;Property;_CenterTxtrColors;CenterTxtrColors;32;1;[HDR];Create;True;0;0;0;False;0;False;0.2792338,0.123398,0.5566038,0;0.1476932,0.01960784,0.5647059,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;235;76.88892,-47.75553;Inherit;False;Property;_CenterNoiseSize;CenterNoiseSize;25;0;Create;True;0;0;0;False;0;False;0;8.28;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;248;-557.7307,-741.3992;Inherit;False;Property;_CenterTxtrMoveUV1;CenterTxtrMoveUV;31;0;Create;True;0;0;0;False;0;False;1;0.75;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;247;-543.3641,-663.6594;Float;False;Property;_CenterTxtrSpeed1;CenterTxtrSpeed;29;0;Create;True;0;0;0;False;0;False;1,0;-0.025,-0.0025;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.Vector4Node;249;-527.5095,-926.4739;Inherit;False;Property;_CenterTxtrTO1;CenterTxtrTO;30;0;Create;True;0;0;0;False;0;False;1,1,1,0;1.64,1.83,1,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;39;-533.2119,-126.8669;Inherit;False;Property;_CenterNoiseMoveUV;CenterNoiseMoveUV;28;0;Create;True;0;0;0;False;0;False;1;1.41;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;370;-532.8028,-6.676048;Float;False;Property;_CenterNoiseSpeed;CenterNoiseSpeed;26;0;Create;True;0;0;0;False;0;False;1,0;0.005,0.005;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.FunctionNode;368;-227.2816,-243.6277;Inherit;True;Tilling&Offset;-1;;36;31c9a21767af2f446848ac2df08d1f18;0;3;66;FLOAT4;1,1,1,0;False;67;FLOAT;-1;False;68;FLOAT2;0,-0.03;False;1;FLOAT2;0
Node;AmplifyShaderEditor.FunctionNode;369;-239.8235,-759.2665;Inherit;True;Tilling&Offset;-1;;37;31c9a21767af2f446848ac2df08d1f18;0;3;66;FLOAT4;1,1,1,0;False;67;FLOAT;-1;False;68;FLOAT2;0,-0.03;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;86;1576.941,-478.8165;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;84;1856.057,25.30996;Inherit;True;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;122;1533.751,704.2869;Inherit;True;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.Vector4Node;119;223.7731,533.7419;Inherit;False;Property;_NebulaTO;NebulaTO;35;0;Create;True;0;0;0;False;0;False;1,1,1,0;0.39,1,1,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;118;216.5742,747.2202;Inherit;False;Property;_NebulaMoveUv;NebulaMoveUv;36;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;114;233.6308,827.3589;Float;False;Property;_NebulaSpeed;NebulaSpeed;34;0;Create;True;0;0;0;False;0;False;0,0.025;-0.005,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.GradientNode;258;958.6611,1013.308;Inherit;False;0;3;2;0.5190458,0.2126647,0.764151,0;0.8584906,0.5545849,0.1417319,0.5000076;0.5215687,0.2156863,0.7647059,1;1,0;1,1;0;1;OBJECT;0
Node;AmplifyShaderEditor.GradientSampleNode;259;1165.107,1013.815;Inherit;True;2;0;OBJECT;;False;1;FLOAT;0;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ScreenPosInputsNode;105;786.4496,1034.227;Float;False;1;False;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ViewDirInputsCoordNode;48;987.2605,1101.568;Inherit;False;World;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.FunctionNode;366;563.9785,727.1459;Inherit;True;Tilling&Offset;-1;;34;31c9a21767af2f446848ac2df08d1f18;0;3;66;FLOAT4;1,1,1,0;False;67;FLOAT;-1;False;68;FLOAT2;0,-0.03;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;185;6614.343,792.5052;Inherit;True;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;186;6896.419,625.494;Inherit;True;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.ClampOpNode;194;7161.792,626.744;Inherit;True;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;COLOR;1,1,1,0;False;1;COLOR;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;195;6365.524,916.5368;Inherit;False;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;98;5209.482,1105.638;Float;False;Property;_AlphaNSpeed;AlphaNSpeed;39;0;Create;True;0;0;0;False;0;False;0,0.025;0,0.04;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.Vector4Node;94;5210.814,826.6105;Inherit;False;Property;_AlphaNTO;AlphaNTO;41;0;Create;True;0;0;0;False;0;False;1,1,1,0;1,1,1,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;364;5902.061,647.0315;Inherit;True;Tilling&Offset;-1;;32;31c9a21767af2f446848ac2df08d1f18;0;3;66;FLOAT4;1,1,1,0;False;67;FLOAT;-1;False;68;FLOAT2;0,-0.03;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;99;5202.061,1019.263;Inherit;False;Property;_AlphaNMoveUv;AlphaNMoveUv;43;0;Create;True;0;0;0;False;0;False;1;-1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;365;5507.096,922.3559;Inherit;True;Tilling&Offset;-1;;33;31c9a21767af2f446848ac2df08d1f18;0;3;66;FLOAT4;1,1,1,0;False;67;FLOAT;-1;False;68;FLOAT2;0,-0.03;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;212;5925.977,-891.3546;Inherit;True;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.ClampOpNode;213;6191.35,-890.1036;Inherit;True;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;COLOR;1,1,1,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;211;5643.902,-724.3419;Inherit;True;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;208;5164.585,-412.1635;Inherit;False;Property;_BordersNoiseSize;BordersNoiseSize;5;0;Create;True;0;0;0;False;0;False;0;20.9;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;209;5423.437,-678.347;Inherit;False;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;375;4959.286,-710.0664;Inherit;True;Tilling&Offset;-1;;38;31c9a21767af2f446848ac2df08d1f18;0;3;66;FLOAT4;1,1,1,0;False;67;FLOAT;-1;False;68;FLOAT2;0,-0.03;False;1;FLOAT2;0
Node;AmplifyShaderEditor.Vector2Node;291;4450.466,-1038.679;Float;False;Property;_BorderSpeed;BorderSpeed;6;0;Create;True;0;0;0;False;0;False;0,0.025;0,-0.05;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.Vector4Node;290;4476.371,-1294.689;Inherit;False;Property;_BorderTO;BorderTO;7;0;Create;True;0;0;0;False;0;False;1,1,1,0;1.02,0.75,1,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;142;6625.529,-915.1317;Inherit;True;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.ColorNode;143;6333.472,-1140.197;Inherit;False;Property;_BordersColor;BordersColor;8;0;Create;True;0;0;0;False;0;False;0,0,0,0;0.3715671,0.08317003,0.8396226,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;363;2882.587,1059.36;Inherit;False;Tilling&Offset;-1;;31;31c9a21767af2f446848ac2df08d1f18;0;3;66;FLOAT4;1,1,1,0;False;67;FLOAT;-1;False;68;FLOAT2;0,-0.03;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;51;7386.574,46.31413;Float;False;True;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;FSGShaders/sha_Void;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;1;Forward;8;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=Transparent=Queue=0;UniversalMaterialType=Unlit;True;3;True;12;all;0;False;True;1;5;False;;10;False;;1;1;False;;10;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;2;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=UniversalForwardOnly;False;False;0;;0;0;Standard;23;Surface;1;638191468007646408;  Blend;0;0;Two Sided;1;0;Forward Only;0;0;Cast Shadows;1;0;  Use Shadow Threshold;0;0;Receive Shadows;1;0;GPU Instancing;1;0;LOD CrossFade;0;0;Built-in Fog;0;0;DOTS Instancing;0;0;Meta Pass;0;0;Extra Pre Pass;0;0;Tessellation;0;0;  Phong;0;0;  Strength;0.5,False,;0;  Type;0;0;  Tess;16,False,;0;  Min;10,False,;0;  Max;25,False,;0;  Edge Length;16,False,;0;  Max Displacement;25,False,;0;Vertex Position,InvertActionOnDeselection;1;0;0;10;False;True;True;True;False;False;True;True;True;True;False;;False;0
Node;AmplifyShaderEditor.SimpleAddOpNode;141;6871.292,9.370968;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;403;4920.094,35.82877;Inherit;True;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;301;3264.412,1053.045;Inherit;True;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;328;3054.248,1297.928;Inherit;False;Property;_RadialNoiseSize;RadialNoiseSize;17;0;Create;True;0;0;0;False;0;False;0;5.61;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;327;2510.498,1160.019;Float;False;Property;_RadialNoiseSpeed;RadialNoiseSpeed;20;0;Create;True;0;0;0;False;0;False;1,0;0.01,0.01;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.RangedFloatNode;289;4440.533,-1120.089;Inherit;False;Property;_BorderMoveUv;BorderMoveUv;9;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;171;6155.361,1161.419;Inherit;False;Property;_AlphaNoiseSize;AlphaNoiseSize;45;0;Create;True;0;0;0;False;0;False;0;2.45;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;139;4724.774,-538.374;Float;False;Property;_NoiseSpeed;NoiseSpeed;10;0;Create;True;0;0;0;False;0;False;0,0.025;0.01,0.05;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.Vector4Node;138;4639.502,-790.2546;Inherit;False;Property;_NoiseTO;NoiseTO;11;0;Create;True;0;0;0;False;0;False;1,1,1,0;1,1,1,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;137;4804.163,-690.5507;Inherit;False;Property;_NoiseUv;NoiseUv;12;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector4Node;38;-496.1906,-343.6229;Inherit;False;Property;_CenterNoiseTO;CenterNoiseTO;27;0;Create;True;0;0;0;False;0;False;1,1,1,0;1,1,1,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;140;5348.028,-1171.906;Inherit;True;Property;_BordersTxtr;BordersTxtr;0;2;[Header];[NoScaleOffset];Create;True;2;BordersTxtr______________________________;;0;0;False;0;False;-1;a2e879e588630da4cbbf459a462a9ac2;656237196ebeaf640b519c8034f9bb8b;True;1;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;31;142.8028,-787.34;Inherit;True;Property;_CenterTxtr;CenterTxtr;24;2;[Header];[NoScaleOffset];Create;True;1;CenterTxtr______________________________;0;0;False;0;False;-1;None;a42a95fa2174bab4e911b06f9e4f8fc0;True;1;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;1;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;104;1153.313,698.0577;Inherit;True;Property;_NebulaTxtr;NebulaTxtr;33;2;[Header];[NoScaleOffset];Create;True;2;Nebula______________________________;;0;0;False;0;False;-1;None;d007e1c2287a706498f45c7fba0c7bc9;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;74;6275.953,619.7171;Inherit;True;Property;_AlphaTxtr;AlphaTxtr;38;2;[Header];[NoScaleOffset];Create;True;1;AlphaNoise______________________________;0;0;False;0;False;-1;a2e879e588630da4cbbf459a462a9ac2;5554e28a2cbad8d4a86e769309f9cb6a;True;1;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.NoiseGeneratorNode;233;326.2521,-288.9053;Inherit;True;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;231;1281.389,-482.514;Inherit;True;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;COLOR;1,1,1,0;False;1;COLOR;0
Node;AmplifyShaderEditor.ColorNode;405;1482.867,1102.476;Inherit;False;Property;_NebulaColor;NebulaColor;37;0;Create;True;0;0;0;False;0;False;0.5215687,0.2117647,0.764706,1;0.1920529,0.1176469,0.490196,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.Vector4Node;373;5549.157,404.7228;Inherit;False;Property;_AlphaTextureTO;AlphaTextureTO;42;0;Create;True;0;0;0;False;0;False;1,1,1,0;1,1,1,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;374;5550.343,580.63;Inherit;False;Property;_AlphaTextureMoveUv;AlphaTextureMoveUv;44;0;Create;True;0;0;0;False;0;False;1;-1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;371;5547.825,683.7513;Float;False;Property;_AlphaTextureSpeed;AlphaTextureSpeed;40;0;Create;True;0;0;0;False;0;False;0,0.025;0,-0.03;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.SimpleAddOpNode;299;3865.296,890.5376;Inherit;True;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.ClampOpNode;300;4130.668,891.7885;Inherit;True;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;COLOR;1,1,1,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;302;3583.219,1057.551;Inherit;True;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;415;4733.262,827.7804;Inherit;True;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.Vector4Node;325;2600.472,890.9494;Inherit;False;Property;_RadialNoiseTO;RadialNoiseTO;18;0;Create;True;0;0;0;False;0;False;1,1,1,0;1,1,1,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;304;3197.19,596.8278;Inherit;True;Property;_RadialTxtr;RadialTxtr;13;2;[Header];[NoScaleOffset];Create;True;1;RadialTxtr______________________________;0;0;False;0;False;-1;None;0bf7b88324a485e429d00b5d241a051d;True;1;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;1;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;326;2497.863,1073.138;Inherit;False;Property;_RadialNoiseMoveUV;RadialNoiseMoveUV;19;0;Create;True;0;0;0;False;0;False;1;10.31;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;329;4437.073,894.0965;Inherit;True;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.OneMinusNode;416;4559.394,752.5117;Inherit;False;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.ColorNode;303;3919.563,640.0732;Inherit;False;Property;_RadialTxtrColors;RadialTxtrColors;22;1;[HDR];Create;True;0;0;0;False;0;False;0.2792338,0.123398,0.5566038,0;0.04850254,0.01308295,0.1320755,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;409;4218.541,534.4036;Inherit;True;Property;_RadialMask;RadialMask;23;1;[NoScaleOffset];Create;True;0;0;0;False;0;False;-1;None;516257369772a0442ac72d58f215762f;True;1;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.Vector2Node;399;1951.339,787.4854;Inherit;False;Property;_RadialTiling;RadialTiling;16;0;Create;True;0;0;0;False;0;False;0,0;1,1;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.Vector2Node;398;2042.566,533.0178;Inherit;False;Property;_RadialTextureCoordination;RadialTextureCoordination;14;0;Create;True;0;0;0;False;0;False;1,1;-5,1;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.Vector2Node;404;2129.385,666.9327;Inherit;False;Property;_RadialTextureSpeed;RadialTextureSpeed;21;0;Create;True;0;0;0;False;0;False;0,0;0.005,0.03;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.TextureCoordinatesNode;397;2169.755,812.3222;Inherit;False;1;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0.5,0.5;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.Vector2Node;400;1948.24,980.976;Inherit;False;Property;_RadialOffset;RadialOffset;15;0;Create;True;0;0;0;False;0;False;0.5,0.5;0.5,0.5;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.FunctionNode;354;2392.244,623.5638;Inherit;True;RadialUVDistortion;-1;;25;051d65e7699b41a4c800363fd0e822b2;0;7;60;SAMPLER2D;_Sampler60354;False;1;FLOAT2;2.83,1;False;11;FLOAT2;0,0;False;65;FLOAT;1;False;68;FLOAT2;1,1;False;47;FLOAT2;0.08,0.43;False;29;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.FunctionNode;376;4918.037,-1143.101;Inherit;True;Tilling&Offset;-1;;39;31c9a21767af2f446848ac2df08d1f18;0;3;66;FLOAT4;1,1,1,0;False;67;FLOAT;-1;False;68;FLOAT2;0,-0.03;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;424;4568.732,-1687.423;Inherit;False;1;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0.5,0.5;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ToggleSwitchNode;427;5245.57,-1590.829;Inherit;True;Property;_RadialUV;RadialUV;1;0;Create;True;0;0;0;False;0;False;0;True;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.Vector2Node;425;4347.222,-1518.769;Inherit;False;Property;_RadialBorderOffset;RadialBorderOffset;4;0;Create;True;0;0;0;False;0;False;0.5,0.5;0.5,0.5;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.Vector2Node;423;4528.362,-1832.812;Inherit;False;Property;_RadialBorderTextureSpeed;RadialBorderTextureSpeed;3;0;Create;True;0;0;0;False;0;False;0,0;0.005,0.03;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.Vector2Node;422;4443.289,-1964.987;Inherit;False;Property;_RadialBorderTextureCoordination;RadialBorderTextureCoordination;2;0;Create;True;0;0;0;False;0;False;1,1;-5,1;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.FunctionNode;426;4791.221,-1876.182;Inherit;True;RadialUVDistortion;-1;;44;051d65e7699b41a4c800363fd0e822b2;0;7;60;SAMPLER2D;_Sampler60426;False;1;FLOAT2;2.83,1;False;11;FLOAT2;0,0;False;65;FLOAT;1;False;68;FLOAT2;1,1;False;47;FLOAT2;0.08,0.43;False;29;FLOAT2;0,0;False;1;FLOAT2;0
WireConnection;230;0;31;0
WireConnection;230;1;234;0
WireConnection;234;0;31;0
WireConnection;234;1;233;0
WireConnection;368;66;38;0
WireConnection;368;67;39;0
WireConnection;368;68;370;0
WireConnection;369;66;249;0
WireConnection;369;67;248;0
WireConnection;369;68;247;0
WireConnection;86;0;231;0
WireConnection;86;1;85;0
WireConnection;84;0;122;0
WireConnection;84;1;86;0
WireConnection;122;0;104;0
WireConnection;122;1;405;0
WireConnection;259;0;258;0
WireConnection;259;1;105;0
WireConnection;366;66;119;0
WireConnection;366;67;118;0
WireConnection;366;68;114;0
WireConnection;185;0;74;0
WireConnection;185;1;195;0
WireConnection;186;0;74;0
WireConnection;186;1;185;0
WireConnection;194;0;186;0
WireConnection;195;0;365;0
WireConnection;195;1;171;0
WireConnection;364;66;373;0
WireConnection;364;67;374;0
WireConnection;364;68;371;0
WireConnection;365;66;94;0
WireConnection;365;67;99;0
WireConnection;365;68;98;0
WireConnection;212;0;140;0
WireConnection;212;1;211;0
WireConnection;213;0;212;0
WireConnection;211;0;140;0
WireConnection;211;1;209;0
WireConnection;209;0;375;0
WireConnection;209;1;208;0
WireConnection;375;66;138;0
WireConnection;375;67;137;0
WireConnection;375;68;139;0
WireConnection;142;0;143;0
WireConnection;142;1;213;0
WireConnection;363;66;325;0
WireConnection;363;67;326;0
WireConnection;363;68;327;0
WireConnection;51;2;141;0
WireConnection;51;3;194;0
WireConnection;141;0;142;0
WireConnection;141;1;403;0
WireConnection;403;0;415;0
WireConnection;403;1;84;0
WireConnection;301;0;363;0
WireConnection;301;1;328;0
WireConnection;140;1;427;0
WireConnection;31;1;369;0
WireConnection;104;1;366;0
WireConnection;74;1;364;0
WireConnection;233;0;368;0
WireConnection;233;1;235;0
WireConnection;231;0;230;0
WireConnection;299;0;304;0
WireConnection;299;1;302;0
WireConnection;300;0;299;0
WireConnection;302;0;304;0
WireConnection;302;1;301;0
WireConnection;415;0;416;0
WireConnection;415;1;329;0
WireConnection;304;1;354;0
WireConnection;329;0;300;0
WireConnection;329;1;303;0
WireConnection;416;0;409;0
WireConnection;397;0;399;0
WireConnection;397;1;400;0
WireConnection;354;68;398;0
WireConnection;354;47;404;0
WireConnection;354;29;397;0
WireConnection;376;66;290;0
WireConnection;376;67;289;0
WireConnection;376;68;291;0
WireConnection;424;1;425;0
WireConnection;427;0;376;0
WireConnection;427;1;426;0
WireConnection;426;68;422;0
WireConnection;426;47;423;0
WireConnection;426;29;424;0
ASEEND*/
//CHKSM=0D66715F556F3D55213FDA3922299EEA106445F9