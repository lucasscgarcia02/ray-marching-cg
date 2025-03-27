const vertexShaderSrc = `#version 300 es
precision highp float;
in vec2 a_position;
void main() {
    gl_Position = vec4(a_position, 0.0, 1.0);
}`;

const fragmentShaderSrc = `#version 300 es
// Créditos
// Boilerplate: https://www.shadertoy.com/view/fdlGWX
// Refração: https://www.shadertoy.com/view/sls3WN

precision highp float;

out vec4 outColor;

uniform vec2 u_resolution;
uniform float u_time;

const int MAX_MARCHING_STEPS = 255;
const float MIN_DIST = 0.0;
const float MAX_DIST = 100.0;
const float PRECISION = 0.001;

struct Surface {
    float sd; // signed distance value
    vec3 col; // color
    bool isTrans;
};

Surface sdSphere(vec3 p, float r, vec3 offset, vec3 col, bool trans) {
    float d = length(p - offset) - r;
    return Surface(d, col, trans);
}

Surface sdFloor(vec3 p, vec3 col, bool trans) {
    float d = p.y + 1.;
    return Surface(d, col, trans);
}

Surface sdCappedCone(vec3 p, vec3 a, vec3 b, float ra, float rb, vec3 col, bool trans) {
    float rba  = rb-ra;
    float baba = dot(b-a,b-a);
    float papa = dot(p-a,p-a);
    float paba = dot(p-a,b-a)/baba;
    float x = sqrt( papa - paba*paba*baba );
    float cax = max(0.0,x-((paba<0.5)?ra:rb));
    float cay = abs(paba-0.5)-0.5;
    float k = rba*rba + baba;
    float f = clamp( (rba*(x-ra)+paba*baba)/k, 0.0, 1.0 );
    float cbx = x-ra - f*rba;
    float cby = paba - f;
    float s = (cbx<0.0 && cay<0.0) ? -1.0 : 1.0;
    float d = s*sqrt( min(cax*cax + cay*cay*baba,
                        cbx*cbx + cby*cby*baba) );
    return Surface(d, col, trans);
}

Surface sdRoundCone(vec3 p, vec3 offset, float r1, float r2, float h, vec3 col, bool trans) {
    p -= offset;
    // sampling independent computations (only depend on shape)
    float b = (r1-r2)/h;
    float a = sqrt(1.0-b*b);

    // sampling dependant computations
    vec2 q = vec2( length(p.xz), p.y );
    float k = dot(q,vec2(-b,a));
    if( k<0.0 ) return Surface(length(q) - r1, col, trans);
    if( k>a*h ) return Surface(length(q-vec2(0.0,h)) - r2, col, trans);
    
    float d = dot(q, vec2(a,b) ) - r1;
    return Surface(d, col, trans);
}

Surface minWithColor(Surface obj1, Surface obj2) {
  if (obj2.sd < obj1.sd) return obj2; // The sd component of the struct holds the "signed distance" value
  return obj1;
}

float smin(float a, float b, float k) {
    float h = clamp(0.5+0.5*(b-a)/k, 0., 1.);
    return mix(b, a, h) - k*h*(1.0-h);
}

Surface sdScene(vec3 p) {
    vec3 s1 = vec3(0, 1.5, -2.6);
    vec3 s2 = vec3(0, 1.5, -2.6);

    float speed = 1.5;
    float height = 0.37;

    s1.y += sin(u_time * speed) * height;
    s2.y += sin((u_time + 10.) * speed) * height;

    Surface sphere1 = sdSphere(p, 0.3, s1, vec3(0, .8, .8), false);
    Surface sphere2 = sdSphere(p, 0.3, s2, vec3(1, 0.58, 0.29), false);
    Surface cCone1 = sdCappedCone(p, vec3(0, 2.25, -2), vec3 (0, 2.75, -2), 0.4, 0.2, vec3(1, 0.58, 0.29), false);
    Surface cCone2 = sdCappedCone(p, vec3(0, 0.25, -2), vec3(0, 0.75, -2), 0.3, 0.6, vec3(1, 0.58, 0.29), false);
    Surface cCone3 = sdCappedCone(p, vec3(0, 0.25, -2), vec3(0, -0.25, -2), 0.3, 0.6, vec3(1, 0.58, 0.29), false);
    Surface rCone = sdRoundCone(p, vec3(0, 1.12, -2), .65, .45, 0.85, vec3(0, .8, .8), true);
    
    Surface sminSphere = Surface(smin(sphere1.sd, sphere2.sd, 0.5), 
                                mix(sphere1.col, sphere2.col, 0.5),
                                false);

    Surface co = minWithColor(sminSphere, rCone);
    co = minWithColor(co, cCone1);
    co = minWithColor(co, cCone2);
    co = minWithColor(co, cCone3);

    vec3 floorColor = vec3(1. + 0.7*mod(floor(p.x) + floor(p.z), 2.0));
    co = minWithColor(co, sdFloor(p, floorColor, false));
    return co;
}

Surface rayMarch(vec3 ro, vec3 rd, float start, float end, float side) {
    float depth = start;
    Surface co; // closest object

    for (int i = 0; i < MAX_MARCHING_STEPS; i++) {
        vec3 p = ro + depth * rd;
        co = sdScene(p);
        co.sd *= side;
        depth += co.sd;
        if (co.sd < PRECISION || depth > end) break;
    }
  
    co.sd = depth;

    return co;
}

vec3 calcNormal(in vec3 p) {
    vec2 e = vec2(1.0, -1.0) * 0.0005; // epsilon
    return normalize(
        e.xyy * sdScene(p + e.xyy).sd +
        e.yyx * sdScene(p + e.yyx).sd +
        e.yxy * sdScene(p + e.yxy).sd +
        e.xxx * sdScene(p + e.xxx).sd);
}

void main() {
    vec2 uv = (gl_FragCoord.xy - .5 * u_resolution) / u_resolution.y;
    vec3 backgroundColor = vec3(0.835, 1, 1);

    vec3 col = vec3(0);
    vec3 ro = vec3(0, 1.25, 2); // ray origin that represents camera position
    vec3 rd = normalize(vec3(uv, -1)); // ray direction

    Surface co = rayMarch(ro, rd, MIN_DIST, MAX_DIST, 1.); // closest object

    if (co.sd > MAX_DIST) {
        col = backgroundColor; // ray didn't hit anything
    } else {
        vec3 p = ro + rd * co.sd; // point on sphere or floor we discovered from ray marching
        vec3 normal = calcNormal(p);

        if (co.isTrans) {
            vec3 pEnter = p - normal * PRECISION * 2.;
            co = rayMarch(pEnter, rd, MIN_DIST, MAX_DIST, -1.); // Inside the object
        }
            
        vec3 lightPosition = vec3(2, 2, 7);
        vec3 lightDirection = normalize(lightPosition - p);

        // Calculate diffuse reflection by taking the dot product of 
        // the normal and the light direction.
        float dif = clamp(dot(normal, lightDirection), 0.3, 1.);

        // Multiply the diffuse reflection value by an orange color and add a bit
        // of the background color to the sphere to blend it more with the background.
        col = dif * co.col + backgroundColor * .2;
    }

    outColor = vec4(col, 1.0); // Aplicando a transparência
}`;

function createShader(gl, type, source) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        console.error(gl.getShaderInfoLog(shader));
        gl.deleteShader(shader);
        return null;
    }
    return shader;
}

function createProgram(gl, vsSource, fsSource) {
    const vertexShader = createShader(gl, gl.VERTEX_SHADER, vsSource);
    const fragmentShader = createShader(gl, gl.FRAGMENT_SHADER, fsSource);
    const program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        console.error(gl.getProgramInfoLog(program));
        gl.deleteProgram(program);
        return null;
    }
    return program;
}

function main() {
    const canvas = document.getElementById("glCanvas");
    const gl = canvas.getContext("webgl2");
    if (!gl) {
        console.error("WebGL2 not supported");
        return;
    }

    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    gl.viewport(0, 0, canvas.width, canvas.height);

    const program = createProgram(gl, vertexShaderSrc, fragmentShaderSrc);
    gl.useProgram(program);

    const positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
        -1, -1,  1, -1,  -1, 1,
        -1,  1,  1, -1,   1, 1
    ]), gl.STATIC_DRAW);

    const vao = gl.createVertexArray();
    gl.bindVertexArray(vao);

    const posLocation = gl.getAttribLocation(program, "a_position");
    gl.enableVertexAttribArray(posLocation);
    gl.vertexAttribPointer(posLocation, 2, gl.FLOAT, false, 0, 0);

    const resolutionLocation = gl.getUniformLocation(program, "u_resolution");
    const timeLocation = gl.getUniformLocation(program, "u_time");

    function render(time) {
        gl.uniform2f(resolutionLocation, canvas.width, canvas.height);
        gl.uniform1f(timeLocation, time * 0.001);
        gl.clear(gl.COLOR_BUFFER_BIT);
        gl.drawArrays(gl.TRIANGLES, 0, 6);
        requestAnimationFrame(render);
    }
    requestAnimationFrame(render);

    window.addEventListener("resize", () => {
        canvas.width = window.innerWidth;
        canvas.height = window.innerHeight;
        gl.viewport(0, 0, canvas.width, canvas.height);
    });
}

main();
