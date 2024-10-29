using UnityEngine;

public class LavaFlow : MonoBehaviour
{
    public float scrollSpeedX = 0.1f;
    public float scrollSpeedY = 0.1f;
    private Renderer rend;

    void Start()
    {
        rend = GetComponent<Renderer>();
    }

    void Update()
    {
        float offsetX = Time.time * scrollSpeedX;
        float offsetY = Time.time * scrollSpeedY;

        // Update texture offset on URP Lit shader
        rend.material.SetTextureOffset("_BaseMap", new Vector2(offsetX, offsetY));
    }
}
