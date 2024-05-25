import type { Context, Config } from "@netlify/edge-functions";

export default async (request: Request, context: Context) => {

  const BLOCKED_USERAGENT = "ccbot|chatgpt-user|gptbot|google-extended|anthropic-ai|claudebot|omgilibot|omgili|facebookbot|diffbot|bytespider|imagesiftbot|cohere-ai";
  const headers = request.headers
  const useragent = headers.get("User-Agent") || ""

  if (RegExp(BLOCKED_USERAGENT).test(useragent.toLowerCase())) {
    console.log(`Blocked access from ${useragent}`)
    return new Response("User-Agent blocked", {
      headers: { "content-type": "text/html" },
      status: 403,
    });
  }

};

export const config: Config = {
  path: "/*",
  excludedPath: ["/*.css", "/*.js"]
};